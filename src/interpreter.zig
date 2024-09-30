const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const StringHashMap = std.StringHashMap;

const Environment = @import("environment.zig");
const Expr = @import("expr.zig").Expr;
const FunctionContext = @import("loxfunction.zig");
const ClassContext = @import("loxclass.zig");
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const Value = @import("expr.zig").Expr.Value;
const debug = @import("debug.zig");
const logger = @import("logger.zig");
const main = @import("main.zig");
const root = @import("root.zig");
const runtimeError = @import("main.zig").runtimeError;

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
    assert(@sizeOf(@This()) == 72);
    assert(@alignOf(@This()) == 8);
}

/// Uses thread-local variables to communicate out-of-band error information.
/// See also:
///   - https://www.reddit.com/r/Zig/comments/wqnd04/my_reasoning_for_why_zig_errors_shouldnt_have_a/
///   - https://github.com/ziglang/zig/issues/2647
/// Key doesn't already exist in environments's variable map.
pub var undeclared_token: Token = undefined;
pub var runtime_token: Token = undefined;

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
};

/// Not actual errors, but a way to propagate return values.
const PropagationException = error{@"return"};

pub const Error = RuntimeError || PropagationException || Environment.Error || Allocator.Error;

const Self = @This();

// TODO: Reason about the relationship between `globals` and `environment`.
pub fn init(allocator: Allocator) Allocator.Error!Interpreter {
    // NOTE: Shared pointer for globals and environment. So if in current
    // scope, when a value is put in `Interpreter.environment`, it is also
    // pointed to globals.
    const globals = try Environment.init(allocator);

    var self: Interpreter = .{
        .allocator = allocator,
        .environment = globals,
        .globals = globals,
        .locals = StringHashMap(i32).init(allocator),
    };

    {
        const clock_callable = try self.allocator.create(Value.LoxCallable);
        clock_callable.* = clockGlobalCallable;
        self.globals.define("clock", .{ .callable = clock_callable }) catch |err| {
            try handleRuntimeError(err);
        };
    }

    return self;
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
        error.wrong_arity => {
            return runtimeError(undeclared_token, "Wrong function arguments arity '{s}'.", .{undeclared_token.lexeme});
        },

        error.non_instance_property => {
            return runtimeError(runtime_token, "Only instances have properties '{s}'.", .{runtime_token.lexeme});
        },

        // PropagationException
        error.@"return" => {
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

fn printValue(writer: anytype, value: Value) void {
    switch (value) {
        .bool => |@"bool"| std.fmt.format(writer, "{}", .{@"bool"}) catch {},
        .nil => std.fmt.format(writer, "nil", .{}) catch {},
        .num => |num| std.fmt.format(writer, "{d}", .{num}) catch {},
        .str => |str| std.fmt.format(writer, "{s}", .{str}) catch {},

        .ret => |ret| std.fmt.format(writer, "{any}", .{ret.toValue()}) catch {},

        .callable => |callable| std.fmt.format(writer, "{s}", .{callable.toString()}) catch {},
        .function => |callable_function| std.fmt.format(writer, "{s}", .{callable_function.toString()}) catch {},
        .class => |callable_class| std.fmt.format(writer, "{s}", .{callable_class.toString()}) catch {},
        .instance => |callable_class_instance| std.fmt.format(writer, "{s}", .{callable_class_instance.toString()}) catch {},
    }
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
        .callable => |callable| blk: { // `Value.LoxCallable` (native)
            if (callable.arity() != args_count) {
                runtime_token = call.paren;
                break :blk Error.wrong_arity;
            }
            break :blk callable.callFn(self, try arguments.toOwnedSlice());
        },
        .function => |function| blk: { // `Value.LoxFunction`
            if (function.arity() != args_count) {
                runtime_token = call.paren;
                break :blk Error.wrong_arity;
            }
            break :blk function.call(self, try arguments.toOwnedSlice());
        },
        //
        //
        // Right now, if you try this:
        //
        //      class Bagel {}
        //      Bagel();
        //
        // You get a runtime error. visitCallExpr() checks to see if the called
        // object implements LoxCallable and reports an error since LoxClass
        // doesn’t. Not yet, that is.
        //
        //
        // SHOULD BE BAGEL INSTANCE
        //
        //      print "file: test.lox";
        //
        //      print "# Class";
        //
        //      class Bagel {};
        //      var bagel = Bagel();
        //      print Bagel; // Prints "Bagel instance".
        //
        // See https://craftinginterpreters.com/classes.html#creating-instances
        //
        //
        //
        //
        .class => |class| blk: {
            logger.warn(.default, @src(), "DOING: .class {any} for call: {any}", .{
                class,
                call.callee,
            });

            if (class.arity() != args_count) {
                runtime_token = call.paren;
                break :blk Error.wrong_arity;
            }
            break :blk class.call(self, try arguments.toOwnedSlice());
        },
        .instance => |_| @panic("Unimplemented"),
        //
        //
        //
        //
        //
        //
        //
        //
        else => blk: {
            defer arguments.deinit();
            runtime_token = call.paren;
            break :blk Error.not_callable;
        },
    };
}

fn visitGetExpr(self: *Self, get: Expr.Get) Error!Value {
    // Evaluate the expression whose property is being accessed.
    const value: Value = try self.evaluate(get.value);

    return switch (value) {
        .instance => |instance| blk: {
            _ = instance;
            // If the object is a LoxInstance, then we ask it to look up the property. It must be time to give LoxInstance some actual state. A map will do fine.
            //
            //   private LoxClass klass;
            //   private final Map<String, Object> fields = new HashMap<>();
            //
            //   LoxInstance(LoxClass klass) {
            // lox/LoxInstance.java, in class LoxInstance
            // Each key in the map is a property name and the corresponding value is the property’s value. To look up a property on an instance:
            //
            //   Object get(Token name) {
            //     if (fields.containsKey(name.lexeme)) {
            //       return fields.get(name.lexeme);
            //     }
            //
            //     throw new RuntimeError(name,
            //         "Undefined property '" + name.lexeme + "'.");
            //   }
            //
            // const out: Value = instance.get(self, get.name);
            // break :blk out;

            break :blk Value.Nil;
        },
        else => blk: {
            // Note: In Lox, only class instances can have properties.
            runtime_token = get.name;
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

fn lookupVariable(self: *Self, name: Token, expr: *Expr) Error!Value {
    _ = name; // autofix

    const distance: i32 = self.locals.get(expr.variable.lexeme) orelse {
        return self.globals.get(expr.variable) catch |err_1| switch (err_1) {
            error.variable_not_declared => self.environment.get(
                expr.variable,
            ) catch |err_2| switch (err_2) {
                error.variable_not_declared => {
                    undeclared_token = expr.variable;
                    return err_2;
                },
                else => |other| other,
            },

            else => |other| other,
        };
    };

    return self.environment.getAt(distance, expr.variable) catch |err| switch (err) {
        error.variable_not_declared => {
            undeclared_token = expr.variable;
            return err;
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
    return switch (expr.*) {
        .assign => |assign| try self.visitAssignExpr(assign),
        .binary => |binary| try self.visitBinaryExpr(binary),
        .call => |call| try self.visitCallExpr(call),
        .get => |get| try self.visitGetExpr(get),
        .grouping => |grouping| try self.evaluate(grouping),
        .literal => |literal| try self.visitLiteralExpr(literal),
        .logical => |logical| try self.visitLogicalExpr(logical),
        .unary => |unary| try self.visitUnaryExpr(unary),
        .variable => |_| try self.visitVariableExpr(expr),
    };
}

// executeBlock()
// Internally discards the block-local environment and restores the previous
// one that was active back at the callsite.
//
// See https://craftinginterpreters.com/functions.html#function-objects
//
/// FIXME: Should closure be a pointer?
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
            var curr_env = Environment.initEnclosing(self.allocator, prev_env);
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
            try self.environment.define(class.name.lexeme, Value.Nil);

            // FIXME: Implement callFn(), arityFn(), ... with
            // LoxInstance.makeLoxClass(self.allocator, class,
            // self.environment);
            //     `Value not callable ''.`

            const cls: *Value.LoxClass = try ClassContext.createLoxClass(
                self.allocator,
                class.name.lexeme,
            );
            // const cls: *Value.LoxClass = try self.allocator.create(Value.LoxClass);
            errdefer self.allocator.destroy(cls);

            try self.environment.assign(class.name, .{ .class = cls });
            logger.warn(scoper, @src(), "Implementing .class. {s}{}", .{ logger.newline, class });
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
        .function => |function| { // `Stmt.Function`
            const fun: *Value.LoxFunction = try FunctionContext.createLoxFunction(
                self.allocator,
                function,
                self.environment,
            );
            try self.environment.define(function.name.lexeme, .{ .function = fun });
        },
        .print_stmt => |expr| {
            printValue(writer, try self.evaluate(expr));
            writer.writeByte('\n') catch return error.io_error; // multi-line
        },
        .return_stmt => |return_stmt| {
            const value = if (return_stmt.value) |val| try self.evaluate(val) else null;
            runtime_return_value = Expr.LoxReturnValue.fromValue(value).ret;
            runtime_token = return_stmt.keyword;
            runtime_error = error.@"return";
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

const clockGlobalCallable: Value.LoxCallable = .{
    .arityFn = struct {
        fn arity() usize {
            return 0;
        }
    }.arity,
    .callFn = struct {
        fn call(_: *Interpreter, _: []Value) Value {
            return .{ .num = (@as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0) };
        }
    }.call,
    .toStringFn = struct {
        fn toString() []const u8 {
            return "<native fn>";
        }
    }.toString,
};

pub fn interpret(self: *Self, stmts: []Stmt, writer: anytype) Allocator.Error!void {
    if (comptime debug.is_trace_interpreter) {
        const is_resolver_feature_flag = !main.g_is_stable_pre_resolver_feature_flag;
        if (comptime !is_resolver_feature_flag) {
            assert(self.environment.values.count() == 0);
        }
        if (self.globals.values.get("clock")) |value| {
            const fun = value.callable;
            assert(@TypeOf(fun) == *Value.LoxCallable);
            assert((fun.arity() == 0) and mem.eql(u8, fun.toString(), "<native fn>"));
            assert(@TypeOf(fun.call(self, &[_]Value{}).num) == f64);
        }
    }
    // defer {
    //     if (self.environment.values.count() != 0) {
    //         self.environment.values.clearAndFree();
    //     }
    //     assert(self.environment.values.count() == 0);
    // }

    for (stmts) |*stmt| {
        _ = self.execute(stmt, writer) catch |err| return handleRuntimeError(err);
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
//
// FIXME:
//      Where errors bubble up: In the interpreter, specifically in
//      lookupVariable, the error bubbles up when the global variable is not
//      found in locals. Since the resolver isn't responsible for resolving
//      globals, you need to ensure the interpreter properly looks up globals
//      without relying on local resolution.
