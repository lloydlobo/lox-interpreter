const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const AutoHashMap = std.AutoHashMap; // const StringHashMap = std.StringHashMap;

const Environment = @import("environment.zig");
const ErrorCode = @import("main.zig").ErrorCode;
const Expr = @import("expr.zig").Expr;
const FunctionContext = @import("loxfunction.zig");
const Logger = @import("logger.zig");
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const Value = @import("expr.zig").Expr.Value;
const debug = @import("debug.zig");
const main = @import("main.zig");
const root = @import("root.zig");
const runtimeError = @import("main.zig").runtimeError;

/// `allocator`:   The arena allocator.
/// `globals`:     Holds a fixed reference to the outermost global environment.
/// `environment`: Tracks current frame and changes as we enter and exit local scopes.
/// `locals`:      Tabular data structure that stores data separately from the objects it relates to.
const Interpreter = @This(); // File struct

allocator: Allocator,
globals: *Environment,
environment: *Environment,
locals: AutoHashMap(*Expr, i32),
simplocals: std.StringHashMap(i32),

/// Uses thread-local variables to communicate out-of-band error information.
/// See also:
///   - https://www.reddit.com/r/Zig/comments/wqnd04/my_reasoning_for_why_zig_errors_shouldnt_have_a/
///   - https://github.com/ziglang/zig/issues/2647
/// Key doesn't already exist in environments's variable map.
pub var undeclared_token: Token = undefined;
pub var runtime_error: Error = undefined;
pub var runtime_return_value: Value = undefined;
pub var runtime_token: Token = undefined;

const Self = @This();

const RuntimeError = error{
    io_error,
    operand_not_number,
    operands_not_numbers,
    operands_neither_numbers_nor_strings,
    wrong_arity,
    not_calable,
};

/// Not actual errors, but a way to propagate return values.
const PropagationException = error{Return};

pub const Error = RuntimeError ||
    PropagationException ||
    Environment.Error ||
    Allocator.Error;

pub fn init(allocator: Allocator) Allocator.Error!Interpreter {
    var self: Interpreter = .{
        .allocator = allocator,
        .environment = try Environment.init(allocator),
        .globals = try Environment.init(allocator),
        .locals = AutoHashMap(*Expr, i32).init(allocator),
        .simplocals = std.StringHashMap(i32).init(allocator),
    };

    {
        const clock_callable = try self.allocator.create(Value.LoxCallable);
        clock_callable.* = clockGlobalCallable;
        self.globals.define("clock", .{ .callable = clock_callable }) catch |err|
            try handleRuntimeError(err);
    }

    return self;
}

pub fn handleRuntimeError(err: Error) Allocator.Error!void {
    return switch (err) {
        error.operand_not_number => runtimeError(runtime_token, "Operand must be a number.", .{}),
        error.operands_not_numbers => runtimeError(runtime_token, "Operands must be numbers.", .{}),
        error.operands_neither_numbers_nor_strings => runtimeError(runtime_token, "Operands must two numbers or two strings.", .{}),
        error.variable_not_declared => runtimeError(undeclared_token, "Undefined variable '{s}'.", .{undeclared_token.lexeme}),
        error.wrong_arity => runtimeError(undeclared_token, "Wrong function arguments arity '{s}'.", .{undeclared_token.lexeme}),
        error.not_calable => runtimeError(undeclared_token, "Value not callable '{s}'.", .{undeclared_token.lexeme}),
        error.io_error => root.exit(.exit_failure, "Encountered i/o error at runtime.", .{}),
        error.Return => Logger.err(.{}, @src(), "Trick to propagate return values with errors '{any}' {any}.", .{ runtime_token, runtime_return_value }),
        else => |other| other,
    };
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
        .callable => |callable| std.fmt.format(writer, "{s}", .{callable.toString()}) catch {},
        .function => |function| std.fmt.format(writer, "{s}", .{function.toString()}) catch {},
        .nil => std.fmt.format(writer, "nil", .{}) catch {},
        .num => |num| std.fmt.format(writer, "{d}", .{num}) catch {},
        .ret => |ret| std.fmt.format(writer, "{any}", .{ret.toValue()}) catch {},
        .str => |str| std.fmt.format(writer, "{s}", .{str}) catch {},
    }
}

//
// Visitor methods
//

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
        .plus => blk: {
            break :blk (if (checkOperands(expr.operator, l, r, .num)) |o| .{
                .num = o.l + o.r,
            } else if (checkOperands(expr.operator, l, r, .str)) |o| .{
                .str = try mem.concat(self.allocator, u8, &[_][]const u8{ o.l, o.r }),
            } else error.operands_neither_numbers_nor_strings);
        },
        .slash => if (opsn) |o| .{ .num = o.l / o.r } else error.operands_not_numbers,
        .star => if (opsn) |o| .{ .num = o.l * o.r } else error.operands_not_numbers,
        else => unreachable,
    };
}

/// Each call needs a new environment for recursion and concurrency.
/// `call()` creates environment, binds parameters, and executes.
fn visitCallExpr(self: *Self, call: Expr.Call) Error!Value {
    const callee = try self.evaluate(call.callee);

    const prev_env = self.environment;
    defer self.environment = prev_env;
    var environment = Environment.initEnclosing(self.allocator, prev_env);
    self.environment = &environment;

    const args_count: usize = call.arguments.len;
    var arguments = try std.ArrayList(Value).initCapacity(self.allocator, args_count);
    errdefer arguments.deinit();
    for (call.arguments) |argument|
        try arguments.append(try self.evaluate(argument));
    assert((arguments.items.len == args_count) and (arguments.capacity == args_count));

    return switch (callee) {
        .callable => |callable| blk: { // `Value.LoxCallable` (native)
            if (callable.arity() != args_count) {
                runtime_token = call.paren;
                break :blk error.wrong_arity;
            }
            break :blk callable.callFn(self, try arguments.toOwnedSlice());
        },
        .function => |function| blk: { // `Value.LoxFunction`
            if (function.arity() != args_count) {
                runtime_token = call.paren;
                break :blk error.wrong_arity;
            }
            break :blk function.call(self, try arguments.toOwnedSlice());
        },
        else => blk: {
            defer arguments.deinit();
            runtime_token = call.paren;
            break :blk error.not_calable;
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

/// Assignment references variables.
fn visitAssignExpr(self: *Self, assign: Expr.Assign) Error!Value {
    const value = try self.evaluate(assign.value);

    // if (comptime main.g_is_stable_feature_flag) {
    //     self.environment.assign(assign.name, value) catch |err| {
    //         undeclared_token = assign.name;
    //         return err;
    //     };
    // } else {
    if (self.locals.get(assign.value)) |distance| {
        _ = self.environment.assignAt(distance, assign.name, value) catch |err| {
            Logger.warn(.{}, @src(), "Failed to assignAt: {any}", .{err}); // └─ IDENTIFIER hello null

            undeclared_token = assign.name;
            return err;
        };
    } else {
        self.globals.assign(assign.name, value) catch |err| {
            Logger.warn(.{}, @src(), "Failed to assign: {any}", .{err}); // └─ IDENTIFIER hello null
            undeclared_token = assign.name;
            return err;
        };
    }
    // }

    return value;
}

fn visitVariableExpr(self: *Self, expr: *Expr) Error!Value {
    if (root.unionPayloadPtr(Token, expr)) |variable| {
        Logger.debug(.{}, @src(), "visitVariableExpr: {any}", .{variable}); // └─ IDENTIFIER hello null
    }

    // if (comptime main.g_is_stable_feature_flag) {
    //     return self.environment.get(expr.variable) catch |err| blk: {
    //         undeclared_token = expr.variable;
    //         break :blk err;
    //     };
    // } else {
    // TODO:
    // https://craftinginterpreters.com/resolving-and-binding.html#accessing-a-resolved-variable
    return self.lookupVariable(expr.variable, expr) catch |err| blk: {
        Logger.warn(.{}, @src(), "Failed to lookup variable.{s}[variable: {s}]{s}[err: {any}]", .{
            Logger.newline,
            expr.variable,
            Logger.newline,
            err,
        });

        break :blk self.environment.get(expr.variable) catch |e| inner_blk: {
            Logger.warn(.{}, @src(), "Failed to get variable value from environment.{s}[variable: {s}]{s}[err: {any}]", .{
                Logger.newline,
                expr.variable,
                Logger.newline,
                e,
            });

            undeclared_token = expr.variable;
            break :inner_blk e;
        };
    };
    // }
}

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
fn lookupVariable(self: *Self, name: Token, expr: *Expr) Error!Value {
    return blk: {
        const distance: i32 = self.locals.get(expr) orelse {
            Logger.warn(.{}, @src(), "Variable not found in locals. Now looking for '{s}' in globals.", .{
                name.lexeme,
            });
            const global_value = self.globals.get(expr.variable) catch |err| outer_blk: {
                Logger.warn(.{}, @src(), "Failed to find variable value from globals.{s}[name: {s}]{s}[err: {any}]", .{ Logger.newline, name.lexeme, Logger.newline, err });
                break :outer_blk self.environment.get(expr.variable) catch |e| inner_blk: {
                    undeclared_token = expr.variable;
                    Logger.err(.{}, @src(), "Failed to find variable value in environment.{s}[name: {s}]{s}[err: {any}]", .{ Logger.newline, name.lexeme, Logger.newline, err });
                    break :inner_blk e;
                };
            };
            Logger.info(.{}, @src(), "Got variable value from interpreters environment instead.{s}[value: {any}]", .{ Logger.newline, global_value });
            break :blk global_value;
        };

        break :blk try self.environment.getAt(distance, expr.variable); // catch |err| blk: {
    };
}

// evaluate()
// Visit methods do a **post-order traversal**—each node evaluates its
// children before doing its own work.
fn evaluate(self: *Self, expr: *Expr) Error!Value {
    const result = switch (expr.*) {
        .assign => |assign| try self.visitAssignExpr(assign),
        .binary => |binary| try self.visitBinaryExpr(binary),
        .call => |call| try self.visitCallExpr(call),
        .grouping => |grouping| try self.evaluate(grouping),
        .literal => |literal| try self.visitLiteralExpr(literal),
        .logical => |logical| try self.visitLogicalExpr(logical),
        .unary => |unary| try self.visitUnaryExpr(unary),
        .variable => |_| try self.visitVariableExpr(expr),
    };

    return result;
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
    switch (stmt.*) { // Visit .****Stmt
        .block => |statements| {
            _ = try self.executeBlock(statements, .{ .new = Environment.Closure.Void }, writer);
        },
        .break_stmt => |_| {
            @panic("Unimplemented");
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
            const fun = try FunctionContext.makeLoxFunction(
                self.allocator,
                function,
                self.environment,
            );
            try self.environment.define(
                function.name.lexeme,
                .{ .function = fun },
            );
        },
        .print_stmt => |expr| {
            printValue(writer, try self.evaluate(expr));
            writer.writeByte('\n') catch return error.io_error; // multi-line
        },
        .return_stmt => |return_stmt| {
            const value = if (return_stmt.value) |val| try self.evaluate(val) else null;
            runtime_return_value = Expr.LoxReturnValue.fromValue(value).ret;
            runtime_token = return_stmt.keyword;
            runtime_error = error.Return;
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
pub fn resolve(self: *Self, expr: *Expr, depth: i32) Allocator.Error!void {
    // Maybe this `locals` does not point to resolvers `locals`??
    try self.locals.put(expr, depth);
}

pub fn interpretExpression(self: *Self, expr: *Expr, writer: anytype) Allocator.Error!void {
    const value = self.evaluate(expr) catch |err| return handleRuntimeError(err);
    printValue(writer, value);
}

pub fn interpret(self: *Self, stmts: []Stmt, writer: anytype) Allocator.Error!void {
    if (comptime debug.is_trace_interpreter) {
        assert(self.environment.values.count() == 0);

        if (self.globals.values.get("clock")) |value| {
            const fun = value.callable;
            assert(@TypeOf(fun) == *Value.LoxCallable);
            assert((fun.arity() == 0) and mem.eql(u8, fun.toString(), "<native fn>"));
            assert(@TypeOf(fun.call(self, &[_]Value{}).num) == f64);
        }
    }
    errdefer self.environment.values.clearAndFree();
    defer {
        if (self.environment.values.count() != 0) self.environment.values.clearAndFree();
        assert(self.environment.values.count() == 0);
    }

    var outputs: std.ArrayList(Value) = undefined;
    if (comptime debug.is_trace_interpreter) {
        outputs = try std.ArrayList(Value).initCapacity(self.allocator, stmts.len);
        errdefer outputs.deinit();
    }

    for (stmts) |*stmt| {
        const value = self.execute(stmt, writer) catch |err| {
            return handleRuntimeError(err);
        };

        if (comptime debug.is_trace_interpreter) {
            try outputs.append(value);
        }
    }
    if (comptime debug.is_trace_interpreter) {
        for (try outputs.toOwnedSlice()) |value| {
            Logger.debug(.{}, @src(), "output: '{any}'", .{value});
        }
    }
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
