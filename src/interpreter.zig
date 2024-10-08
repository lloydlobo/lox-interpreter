const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const StringHashMap = std.StringHashMap;

const AstPrinter = @import("astprinter.zig");
const Callable = @import("callable.zig");
const Class = @import("class.zig");
const Environment = @import("environment.zig");
const Expr = @import("expr.zig").Expr;
const Function = @import("function.zig");
const ReturnValue = @import("value.zig").Value.Return;
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const Value = @import("value.zig").Value;
const debug = @import("debug.zig");
const logger = @import("logger.zig");
const main = @import("main.zig");
const root = @import("root.zig");
const runtimeError = @import("main.zig").runtimeError;

const loxbuiltin = @import("builtin/root.zig");

const Interpreter = @This(); // File struct

/// `allocator`:   The arena allocator.
allocator: Allocator,
/// `globals`:     Holds a fixed reference to the outermost global environment.
globals: *Environment,
/// `environment`: Tracks current frame and changes as we enter and exit local scopes.
environment: *Environment,
/// `locals`:      Tabular data structure that stores data separately from the objects it relates to.
locals: StringHashMap(i32),

comptime {
    // assert(@sizeOf(@This()) == 72);
    assert(@alignOf(@This()) == 8);
}

/// Uses thread-local variables to communicate out-of-band error information.
///
/// See also:
///   - https://www.reddit.com/r/Zig/comments/wqnd04/my_reasoning_for_why_zig_errors_shouldnt_have_a/
///   - https://github.com/ziglang/zig/issues/2647
/// Key doesn't already exist in environments's variable map.
pub var runtime_token: Token = undefined;
pub var undeclared_token: Token = undefined;
pub var runtime_error: Error = undefined;
pub var runtime_return_value: Value = undefined;

const RuntimeError = error{
    io_error,

    operand_not_number,
    operands_not_numbers,
    operands_neither_numbers_nor_strings,

    not_callable,
    wrong_arity,

    non_instance_property,
    non_instance_field,
    undefined_property,
};

/// Not actual errors, but a way to propagate return values.
pub const PropagationException = error{
    /// Error indicates that a procedure returned either nil or `Value`.
    return_value,
};

pub const Error = RuntimeError || PropagationException || Environment.Error || Allocator.Error;

const Self = @This();

// TODO: Reason about the relationship between `globals` and `environment`.
pub fn init(allocator: Allocator) Allocator.Error!Interpreter {
    // Shared pointer for globals and environment. So if in current scope, when
    // a value is put in `Interpreter.environment`, it is also pointed to
    // globals.
    const globals = try Environment.init(allocator);

    var self: Interpreter = .{
        .allocator = allocator,
        .environment = globals,
        .globals = globals,
        .locals = StringHashMap(i32).init(allocator),
    };

    {
        const noop: *Callable = try Callable.init(self.allocator);
        errdefer noop.destroy(self.allocator);
        noop.*.vtable = loxbuiltin.default_vtable;

        const clock: *Callable = try Callable.init(self.allocator);
        errdefer clock.destroy(self.allocator);
        clock.*.vtable = loxbuiltin.clock_vtable;

        assert((noop.vtable.arity(noop) == 0) and
            mem.eql(u8, "<native fn>", noop.vtable.toString(noop)) and
            (try noop.vtable.call(noop, &self, &.{})).nil == Value.Nil.nil);

        assert((clock.vtable.arity(clock) == 0) and
            mem.eql(u8, "<native fn>", clock.vtable.toString(clock)) and
            ((try (clock.vtable.call(clock, &self, &.{}))).num > 0.0));

        self.globals.define("noop", .{ .callable = noop }) catch |err| try handleRuntimeError(err);
        self.globals.define("clock", .{ .callable = clock }) catch |err| try handleRuntimeError(err);
    }

    return self;
}

pub fn panicRuntimeError(err: Error, token: Token) noreturn {
    runtime_token = token;
    runtime_error = err;
    handleRuntimeError(err) catch |e| {
        root.exit(.exit_failure, "{any}", .{e});
    };
    root.exit(.exit_failure, "{any}", .{err});
}

pub fn handleRuntimeError(err: Error) Allocator.Error!void {
    switch (err) {
        error.io_error => {
            return root.exit(.exit_failure, "Encountered i/o error at runtime.", .{});
        },

        error.operand_not_number => {
            return runtimeError(runtime_token, "Operand must be a number.", .{});
        },
        error.operands_neither_numbers_nor_strings => {
            return runtimeError(runtime_token, "Operands must two numbers or two strings.", .{});
        },
        error.operands_not_numbers => {
            return runtimeError(runtime_token, "Operands must be numbers.", .{});
        },

        error.not_callable => {
            return runtimeError(undeclared_token, "Value not callable '{s}'.", .{undeclared_token.lexeme});
        },
        error.wrong_arity => { // used to use undeclared_token... but if call() is processed at runtime |> then we missed the token that errors
            return runtimeError(undeclared_token, "Wrong function arguments arity '{s}'.", .{runtime_token.lexeme});
        },

        error.non_instance_field => {
            return runtimeError(runtime_token, "Only instances have fields '{s}'.", .{runtime_token.lexeme});
        },
        error.non_instance_property => {
            return runtimeError(runtime_token, "Only instances have properties '{s}'.", .{runtime_token.lexeme});
        },
        error.undefined_property => {
            return runtimeError(runtime_token, "Undefined property '{s}'.", .{runtime_token.lexeme});
        },

        // PropagationException
        error.return_value => {
            return logger.err(.default, @src(), "Trick to propagate return values with errors '{any}' {any}.", .{
                runtime_token,
                runtime_return_value,
            });
        },

        // Environment.Error
        error.variable_not_declared => {
            return runtimeError(undeclared_token, "Undefined variable '{s}'.", .{undeclared_token.lexeme});
        },

        else => |other| return other,
    }
}

fn printValue(writer: anytype, value: Value) void {
    switch (value) {
        .bool => |@"bool"| std.fmt.format(writer, "{}", .{@"bool"}) catch {},
        .nil => std.fmt.format(writer, "nil", .{}) catch {},
        .num => |num| std.fmt.format(writer, "{d}", .{num}) catch {},
        .str => |str| std.fmt.format(writer, "{s}", .{str}) catch {},

        .ret => |ret| std.fmt.format(writer, "{any}", .{ret.toValue()}) catch {},

        .callable => |callable| std.fmt.format(writer, "{s}", .{callable.toString()}) catch {},

        .function => |function| std.fmt.format(writer, "{s}", .{function.callable.toString()}) catch {},
        .class => |class| std.fmt.format(writer, "{s}", .{class.callable.toString()}) catch {},
        .instance => |class_instance| std.fmt.format(writer, "{s}", .{class_instance.callable.toString()}) catch {},
    }
}

/// Matches operator type with kind and returns its type.
fn checkOperand(
    operator: Token,
    operand: Value,
    comptime kind: std.meta.FieldEnum(Value),
) ?std.meta.fieldInfo(Value, kind).type {
    return switch (operand) {
        kind => |x| x,
        inline else => {
            runtime_token = operator;
            return null;
        },
    };
}

fn checkOperands(
    operator: Token,
    left: Value,
    right: Value,
    comptime kind: std.meta.FieldEnum(Value),
) ?struct {
    l: std.meta.fieldInfo(Value, kind).type,
    r: std.meta.fieldInfo(Value, kind).type,
} {
    return .{
        .l = checkOperand(operator, left, kind) orelse return null,
        .r = checkOperand(operator, right, kind) orelse return null,
    };
}

/// `false` and `nil` are falsey, and everything else is truthy.
fn isTruthy(value: Value) bool {
    return switch (value) {
        .nil => false,
        .bool => |x| x,
        inline else => true,
    };
}

fn isEqual(a: Value, b: Value) bool {
    switch (a) {
        .str => |l| switch (b) {
            .str => |r| return mem.eql(u8, l, r),
            else => {},
        },
        else => {},
    }

    return std.meta.eql(a, b);
}

//
// Visitor methods
//

fn visitAssignExpr(self: *Self, assign: Expr.Assign) Error!Value {
    const value: Value = try self.evaluate(assign.value); // assignment references variables.
    const token: Token = assign.name;

    if (self.locals.get(token.lexeme)) |distance| { // avoid exta work by matching cached value via depth.
        self.environment.assignAt(distance, token, value) catch |err| {
            undeclared_token = token;
            return err;
        };

        return value; // success
    }

    self.globals.assign(token, value) catch |err_1| switch (err_1) {
        error.variable_not_declared => {
            self.environment.assign(token, value) catch |err_2| {
                undeclared_token = token;
                return err_2;
            };
            return value; // success
        },
        else => |other| return other,
    };

    return value; // success

}

fn visitBinaryExpr(self: *Self, expr: Expr.Binary) Error!Value {
    const l = try self.evaluate(expr.left);
    const r = try self.evaluate(expr.right);
    const opsn = checkOperands(expr.operator, l, r, .num);

    return switch (expr.operator.type) {
        .bang_equal => .{ .bool = !isEqual(l, r) },
        .equal_equal => .{ .bool = isEqual(l, r) },
        .greater => if (opsn) |o| .{ .bool = o.l > o.r } else error.operands_not_numbers,
        .greater_equal => if (opsn) |o| .{ .bool = o.l >= o.r } else error.operands_not_numbers,
        .less => if (opsn) |o| .{ .bool = o.l < o.r } else error.operands_not_numbers,
        .less_equal => if (opsn) |o| .{ .bool = o.l <= o.r } else error.operands_not_numbers,
        .minus => if (opsn) |o| .{ .num = o.l - o.r } else error.operands_not_numbers,
        .plus => (if (opsn) |o| .{
            .num = o.l + o.r,
        } else if (checkOperands(expr.operator, l, r, .str)) |o| .{
            .str = try mem.concat(self.allocator, u8, &[_][]const u8{ o.l, o.r }),
        } else error.operands_neither_numbers_nor_strings),
        .slash => if (opsn) |o| .{ .num = o.l / o.r } else error.operands_not_numbers,
        .star => if (opsn) |o| .{ .num = o.l * o.r } else error.operands_not_numbers,
        else => unreachable,
    };
}

/// Each call needs a new environment for recursion and concurrency.
/// `call()` creates environment, binds parameters, and executes.
fn visitCallExpr(self: *Self, call: Expr.Call) Error!Value {
    const callee: Value = try self.evaluate(call.callee);

    const prev_env: *Environment = self.environment;
    defer self.environment = prev_env;
    var environment = Environment.initEnclosing(self.allocator, prev_env);
    self.environment = &environment;

    const args_count: usize = call.arguments.len;
    var arguments = try std.ArrayList(Value).initCapacity(
        self.allocator,
        args_count,
    );
    errdefer arguments.deinit();

    for (call.arguments) |argument| {
        try arguments.append(try self.evaluate(argument));
    }
    assert((arguments.items.len == args_count) and (arguments.capacity == args_count));

    return switch (callee) {
        .callable => |callable| blk: {
            if (callable.arity() != args_count) {
                runtime_token = switch (call.callee.*) {
                    .variable => |variable| variable,
                    .get => |get| get.name,
                    else => unreachable,
                };
                break :blk Error.wrong_arity;
            }
            break :blk callable.call(self, try arguments.toOwnedSlice());
        },
        .function => |function| blk: {
            if (function.callable.arity() != args_count) {
                runtime_token = call.paren; // FIXME: options?
                runtime_token = switch (call.callee.*) {
                    .variable => |variable| variable,
                    .get => |get| get.name,
                    else => unreachable,
                };
                break :blk Error.wrong_arity;
            }
            break :blk function.callable.call(self, try arguments.toOwnedSlice());
        },
        .class => |class| blk: {
            assert(mem.eql(u8, class.name.lexeme, call.callee.variable.lexeme));
            assert(call.callee.variable.literal == null);

            if (class.callable.arity() != args_count) {
                runtime_token = call.paren; // FIXME: options?
                runtime_token = call.callee.variable;
                break :blk Error.wrong_arity;
            }
            break :blk try class.callable.call(self, try arguments.toOwnedSlice());
        },
        .instance => |instance| {
            _ = instance; // autofix

            @panic("Unimplemented");
        },
        else => blk: {
            defer arguments.deinit();
            runtime_token = call.paren;
            break :blk Error.not_callable;
        },
    };
}

// In theory, we can now read properties on objects. But since there’s no way
// to actually stuff any state into an instance, there are no fields to access.
// Before we can test out reading, we must support writing.
fn visitGetExpr(self: *Self, expr: Expr.Get) Error!Value {
    const object: Value = try self.evaluate(expr.object);

    return switch (object) {
        .instance => |instance| blk: {
            logger.info(.default, @src(),
                \\Visiting get expression (DOING)
                \\{s}Class: '{s}'.
                \\{s}Instance Get Expr: '{any}'.
            , .{ logger.indent, instance.callable.toString(), logger.indent, expr });
            break :blk instance.get(expr.name) catch |err| {
                runtime_token = expr.name;
                break :blk err;
            };
        },
        else => blk: { // Note: In Lox, only class instances can have properties.
            runtime_token = expr.name;
            break :blk RuntimeError.non_instance_property;
        },
    };
}

fn visitLiteralExpr(self: *Self, literal: Value) Error!Value {
    return switch (literal) {
        .str => |str| .{ .str = try self.allocator.dupe(u8, str) },
        inline else => literal,
    };
}

fn visitLogicalExpr(self: *Self, expr: Expr.Logical) Error!Value {
    const l = try self.evaluate(expr.left);

    if (expr.operator.type == .@"or") {
        if (isTruthy(l)) {
            return l;
        }
    } else {
        if (!isTruthy(l)) {
            return l;
        }
    }

    return try self.evaluate(expr.right);
}

// Evaluate the expression whose property is being accessed.
/// Raise a runtime error if it’s not an instance of a class.
/// Evaluate the value.
fn visitSetExpr(self: *Self, expr: Expr.Set) Error!Value {
    const object: Value = try self.evaluate(expr.object);

    return switch (object) {
        .instance => |instance| blk: {
            const value = try self.evaluate(expr.value);
            logger.info(.default, @src(),
                \\Visited after evaluating expr.value in Parser.assignment (Expr.Get -> Expr.Set).
                \\{s}expr.name: '{}'.
                \\{s}value: '{}'.
            , .{
                logger.indent, expr.name,
                logger.indent, value,
            });

            instance.set(expr.name, value) catch |err| break :blk err;

            break :blk value;
        },
        else => blk: {
            // Note: In Lox, only class instances can have properties.
            runtime_token = expr.name;
            break :blk RuntimeError.non_instance_field;
        },
    };
}

fn visitThisExpr(self: *Self, expr: Expr.This) Error!Value {
    // .{ .this = expr }
    const this = try self.allocator.create(Expr);
    errdefer self.allocator.destroy(this);

    this.* = .{ .this = expr };
    return try self.lookupVariable(expr.keyword, this);
}

fn visitUnaryExpr(self: *Self, expr: Expr.Unary) Error!Value {
    const r = try self.evaluate(expr.right);

    return switch (expr.operator.type) {
        .minus => .{ .num = -(checkOperand(expr.operator, r, .num) orelse return error.operand_not_number) },
        .bang => .{ .bool = !isTruthy(r) },
        inline else => unreachable,
    };
}

fn visitVariableExpr(self: *Self, expr: *Expr) Error!Value {
    return self.lookupVariable(expr.variable, expr) catch |err_1| switch (err_1) {
        error.variable_not_declared => self.environment.get(expr.variable) catch |err_2| blk: {
            undeclared_token = expr.variable;
            break :blk err_2;
        },
        else => |other| other,
    };
}

// NOTE: Leaving this here as a reminder for [issue #5](https://github.com/lloydlobo/crafting-interpreters-zig/issues/5)
//
// FIXED: While solving the above, got following error:
//       interpreter.zig:lookupVariable:527:27: warn: Looking up variable 'THIS this null'.
//
//   thread 382539 panic: reached unreachable code
//      environment = environment.enclosing orelse unreachable;
//                                                 ^
//      src/environment.zig:99:29: 0x10ead01 in getAt (main)
//      return try self.ancestor(distance).get(name);
// FIXED: Assume this is due to creating binding environment (via function
//        closure), but not initEnclosing it to any:
//
//      defer self.closure.enclosing = environment;
fn lookupVariable(self: *Self, name: Token, expr: *Expr) Error!Value {
    logger.info(.default, @src(), "Looking up variable '{}'.", .{name});

    const distance: i32 = self.locals.get(name.lexeme) orelse {
        return self.globals.get(name) catch |err| switch (err) {
            error.variable_not_declared => self.environment.get(expr.variable) catch |e| switch (e) {
                error.variable_not_declared => {
                    undeclared_token = name;
                    return e; // bail with error
                },
                else => |other| other,
            },
            else => |other| other,
        };
    };

    return self.environment.getAt(distance, name) catch |err| switch (err) {
        error.variable_not_declared => blk: {
            undeclared_token = name;
            break :blk err;
        },
        else => |other| other,
    };
}

fn lookupVariableOld(self: *Self, name: Token, expr: *Expr) Error!Value {
    return blk: {
        const distance: i32 = self.locals.get(expr.variable.lexeme) orelse {
            logger.info(.default, @src(), "Variable not found in simplocals. Now looking for '{s}' in globals...", .{
                name.lexeme,
            });
            const global_value: Value = self.globals.get(expr.variable) catch |err| {
                logger.warn(.default, @src(), "Failed to find variable value from globals. Now looking in environment...{s}[name: {s}]{s}[err: {any}]", //
                    .{ logger.newline, name.lexeme, logger.newline, err });
                break :blk self.environment.get(expr.variable) catch |e| inner_blk: {
                    logger.err(.default, @src(), "Failed to find variable value in environment.{s}[name: {s}]{s}[err: {any}]", //
                        .{ logger.newline, name.lexeme, logger.newline, e });
                    undeclared_token = expr.variable;
                    break :inner_blk e;
                };
            };
            logger.info(.default, @src(), "Yay! Found variable value in globals.{s}[{s}]", //
                .{ logger.newline, global_value });
            break :blk global_value;
        };

        break :blk self.environment.getAt(distance, expr.variable) catch |err| {
            logger.warn(.default, @src(), "Failed to getAt variable value from environment. Now looking in environment...{s}[name: {s}]{s}[err: {any}]", //
                .{ logger.newline, name.lexeme, logger.newline, err });
            break :blk self.environment.get(expr.variable) catch |e| inner_blk: {
                logger.err(.default, @src(), "Failed to find variable value in environment.{s}[name: {s}]{s}[err: {any}]", //
                    .{ logger.newline, name.lexeme, logger.newline, e });
                undeclared_token = expr.variable;
                break :inner_blk e;
            };
        };
    };
}

// evaluate()
// Visit methods do a **post-order traversal**—each node evaluates its
// children before doing its own work.
fn evaluate(self: *Self, expr: *Expr) Error!Value {
    if (comptime debug.is_trace_interpreter and debug.is_trace_astprinter) {
        AstPrinter.debugOtherVariants(root.stderr().writer(), expr) catch |err| {
            root.exit(.exit_failure, "Failed to print ast expression '{s}': {any}", .{
                expr.toString(), err,
            });
        };
    }
    return switch (expr.*) {
        .assign => |assign| try self.visitAssignExpr(assign),
        .binary => |binary| try self.visitBinaryExpr(binary),
        .call => |call| try self.visitCallExpr(call),
        .get => |get| try self.visitGetExpr(get),
        .grouping => |grouping| try self.evaluate(grouping),
        .literal => |literal| try self.visitLiteralExpr(literal),
        .logical => |logical| try self.visitLogicalExpr(logical),
        .set => |set| try self.visitSetExpr(set),
        .this => |this| try self.visitThisExpr(this),
        .unary => |unary| try self.visitUnaryExpr(unary),
        .variable => |_| try self.visitVariableExpr(expr),
    };
}

// executeBlock()
// Internally discards the block-local environment and restores the previous
// one that was active back at the callsite.
//
// See https://craftinginterpreters.com/functions.html#function-objects
pub fn executeBlock(
    self: *Self,
    body: []Stmt,
    closure: Environment.Closure,
    writer: anytype,
) Error!void {
    const prev_env = self.environment;
    defer self.environment = prev_env;

    return switch (closure) {
        .existing => |existing| {
            self.environment = existing;
            for (body) |*statement| {
                _ = try self.execute(statement, writer);
            }
        },
        .new => {
            var curr_env: Environment = Environment.initEnclosing(self.allocator, prev_env);
            self.environment = &curr_env;
            for (body) |*stmt| {
                _ = try self.execute(stmt, writer);
            }
        },
    };
}

pub fn execute(self: *Self, stmt: *Stmt, writer: anytype) Error!Value {
    const scoper: logger.Scoper = .{ .scope = .{ .name = @src().fn_name } };

    switch (stmt.*) { // Visit .****Stmt
        .block => |statements| {
            _ = try self.executeBlock(statements, .{ .new = Environment.Closure.Void }, writer);
        },
        .break_stmt => |_| {
            @panic("Unimplemented");
        },
        .class => |class| {
            logger.info(scoper, @src(),
                \\Visiting class statement declaration.
                \\{s}Capturing class name in a variable in the current environment.
                \\{s}Converting class syntax node to runtime representation of a callable class '{s}'
                \\{s}Storing class value (object) in previously declared variable in the current environment.
                \\{s}Completed two-stage variable binding process to allow references to the class inside its own methods.
            , .{
                logger.indent, logger.indent, class.name.lexeme,
                logger.indent, logger.indent,
            });

            try self.environment.define(class.name.lexeme, Value.Nil);

            var methods = StringHashMap(Function).init(self.allocator);
            for (class.methods) |method| {
                root.assume(@TypeOf(method) == Stmt.Function, .allow);
                const is_initializer = Function.is_init_method(&method);
                if (comptime debug.is_testing) {
                    if (is_initializer) {
                        root.assume(mem.eql(u8, method.name.lexeme, "init"), .allow);
                    }
                }
                logger.warn(.default, @src(),
                    \\method: {}, is_initializer in .class: {}.
                , .{ method.name, is_initializer });

                const fun = try Function.init(self.allocator, method, self.environment, is_initializer);
                if (comptime debug.is_testing) {
                    root.assume(@TypeOf(fun) != @TypeOf(method), .allow);
                }

                try methods.put(method.name.lexeme, fun.*);
            }

            const cls: *Class = try Class.init(self.allocator, class.name, methods);
            errdefer cls.destroy(self.allocator);
            try self.environment.assign(class.name, .{ .class = cls });
            logger.info(scoper, @src(),
                \\Executed and assigned class. {s}{}
            , .{ logger.newline, class });
        },
        .expr_stmt => |expr| {
            _ = try self.evaluate(expr);
        },
        .if_stmt => |if_stmt| {
            _ = if (isTruthy(try self.evaluate(if_stmt.condition)))
                try self.execute(if_stmt.then_branch, writer)
            else if (if_stmt.else_branch) |else_branch|
                try self.execute(else_branch, writer);
        },
        .function => |function| {
            logger.info(scoper, @src(),
                \\Visiting function statement declaration.
                \\{s}Creating callable function '{s}'.
                \\{s}Capturing function in current environment.
            , .{ logger.indent, function.name.lexeme, logger.indent });
            const is_initializer = false; // User may use function with name "init". So no "this" to return.
            const fun = try Function.init(self.allocator, function, self.environment, is_initializer);
            try self.environment.define(function.name.lexeme, .{ .function = fun });
        },
        .print_stmt => |expr| {
            printValue(writer, try self.evaluate(expr));
            writer.writeByte('\n') catch return error.io_error; // multi-line
        },
        .return_stmt => |return_stmt| {
            const value = if (return_stmt.value) |val| try self.evaluate(val) else null;
            runtime_return_value = switch (ReturnValue.fromValue(value)) {
                .nil => Value.Nil,
                .ret => |ret| ret,
            };
            runtime_token = return_stmt.keyword;
            runtime_error = error.return_value;
            return runtime_error; // trick to propagate return values across call stack
        },
        .var_stmt => |var_stmt| {
            const value = if (var_stmt.initializer) |expr|
                try self.evaluate(expr)
            else
                Value.Nil;
            try self.environment.define(var_stmt.name.lexeme, value);
        },
        .while_stmt => |while_stmt| {
            while (isTruthy(try self.evaluate(while_stmt.condition))) {
                _ = try self.execute(while_stmt.body, writer);
            }
        },
    }

    return Value.Nil;
}

pub fn resolve(self: *Self, expr: *Expr, name: Token, depth: i32) Allocator.Error!void {
    _ = expr;
    try self.locals.put(name.lexeme, depth);
}

pub fn interpretExpression(self: *Self, expr: *Expr, writer: anytype) Allocator.Error!void {
    const value = self.evaluate(expr) catch |err| return handleRuntimeError(err);
    printValue(writer, value);
}

pub fn interpret(self: *Self, stmts: []Stmt, writer: anytype) Allocator.Error!void {
    if (comptime debug.is_trace_interpreter) {
        if (self.globals.values.get("clock")) |value| {
            const fun = value.callable;
            assert(@TypeOf(fun) == *Value.CallableValue);
            assert((fun.arity() == 0) and mem.eql(u8, fun.toString(), "<native fn>"));
            assert(@TypeOf((try fun.call(self, &[_]Value{})).num) == f64);
        }
    }

    for (stmts) |*stmt| {
        _ = self.execute(stmt, writer) catch |err| {
            return handleRuntimeError(err);
        };
    }
}

// resolve()
// Each time `Resolver` visits a variable, it tells the interpreter how many
// scopes there are between the current scope and the scope where the variable
// is defined. At runtime, this corresponds exactly to the number of
// environments between the current one and the enclosing one where the
// interpreter can find the variable’s value. The resolver hands that number
// to the interpreter by calling this function.
//
// See https://craftinginterpreters.com/resolving-and-binding.html#interpreting-resolved-variables
//
// Our interpreter now has access to each variable’s resolved location.
// Finally, we get to make use of that. We replace the visit method for
// variable expressions with this:
//
// FIXME: The resolve() function is present, but it's not clear from this code
// snippet how it's integrated into the overall interpretation process.
// * You should ensure that self.globals.get() in lookupVariable() and
//   self.globals.assign() in visitAssignExpr() are properly defined to handle
//   global variable retrieval and assignment.
// * The fallbacks and error handling are good, but ensure that if a variable
//   is missing in both locals and globals, an appropriate runtime error is
//   raised to catch the case where it's truly undeclared.

// lookupVariable()
// There are a couple of things going on here. First, we look up the resolved
// distance in the map. Remember that we resolved only local variables. Globals
// are treated specially and don’t end up in the map (hence the name locals).
// So, if we don’t find a distance in the map, it must be global. In that case,
// we look it up, dynamically, directly in the global environment.
