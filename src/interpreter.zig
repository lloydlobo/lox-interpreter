const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;

const debug = @import("debug.zig");
const Environment = @import("environment.zig");
const ErrorCode = @import("main.zig").ErrorCode;
const Expr = @import("expr.zig").Expr;
const Value = Expr.Value;
const LoxCallable = Value.LoxCallable;
const LoxFunction = Value.LoxFunction;
const makeLoxFunction = @import("loxfunction.zig").makeLoxFunction;
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const root = @import("root.zig");
const runtimeError = @import("main.zig").runtimeError;

const clockGlobalCallable: LoxCallable = .{
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

const Interpreter = @This(); // File struct

allocator: Allocator,
/// Holds a fixed reference to the outermost global environment.
globals: *Environment,
/// Tracks current frame and changes as we enter and exit local scopes.
environment: *Environment,

/// Uses thread-local variables to communicate out-of-band error information.
/// See also:
///   - https://www.reddit.com/r/Zig/comments/wqnd04/my_reasoning_for_why_zig_errors_shouldnt_have_a/
///   - https://github.com/ziglang/zig/issues/2647
pub var runtime_error: Error = undefined;
pub var runtime_return_value: Value = undefined;
pub var runtime_token: Token = undefined;
/// Key doesn't already exist in environments's variable map.
pub var undeclared_token: Token = undefined;

const Self = @This();

const RuntimeError = error{
    IoError,
    OperandNotNumber,
    OperandsNotNumbers,
    OperandsNeitherNumbersNorStrings,
    WrongArity,
    NotCallable,
};
/// These are not actual errors, but a way to propagate stuff like return values.
const PropagationException = error{Return};

pub const Error = Allocator.Error || Environment.Error || RuntimeError || PropagationException;

pub fn init(allocator: Allocator) Allocator.Error!Interpreter {
    var self: Interpreter = .{
        .allocator = allocator,
        .environment = try Environment.init(allocator),
        .globals = try Environment.init(allocator),
    };

    {
        const clock_callable = try self.allocator.create(LoxCallable);
        clock_callable.* = clockGlobalCallable;
        self.globals.define("clock", .{ .callable = clock_callable }) catch |err|
            try handleRuntimeError(err);
    }

    return self;
}

pub fn handleRuntimeError(err: Error) Allocator.Error!void {
    return switch (err) {
        error.OperandNotNumber => runtimeError(runtime_token, "Operand must be a number.", .{}),
        error.OperandsNotNumbers => runtimeError(runtime_token, "Operands must be numbers.", .{}),
        error.OperandsNeitherNumbersNorStrings => runtimeError(runtime_token, "Operands must two numbers or two strings.", .{}),
        error.VariableNotDeclared => runtimeError(undeclared_token, "Undefined variable '{s}'.", .{undeclared_token.lexeme}),
        error.WrongArity => runtimeError(undeclared_token, "Wrong function arguments arity '{s}'.", .{undeclared_token.lexeme}),
        error.NotCallable => runtimeError(undeclared_token, "Value not callable '{s}'.", .{undeclared_token.lexeme}),
        error.IoError => root.exit(1, "Encountered i/o error at runtime.", .{}),
        error.Return => root.tracesrc(@src(), "Trick to propagate return values with errors '{any}' {any}.", .{ runtime_token, runtime_return_value }),
        else => |other| other,
    };
}

fn visitBinaryExpr(self: *Self, expr: Expr.Binary) Error!Value {
    const l = try self.evaluate(expr.left);
    const r = try self.evaluate(expr.right);
    const opsn = checkOperands(expr.operator, l, r, .num);

    return switch (expr.operator.type) {
        .bang_equal => .{ .bool = !isEqual(l, r) },
        .equal_equal => .{ .bool = isEqual(l, r) },
        .greater => if (opsn) |o| .{ .bool = o.l > o.r } else error.OperandsNotNumbers,
        .greater_equal => if (opsn) |o| .{ .bool = o.l >= o.r } else error.OperandsNotNumbers,
        .less => if (opsn) |o| .{ .bool = o.l < o.r } else error.OperandsNotNumbers,
        .less_equal => if (opsn) |o| .{ .bool = o.l <= o.r } else error.OperandsNotNumbers,
        .minus => if (opsn) |o| .{ .num = o.l - o.r } else error.OperandsNotNumbers,
        .plus => blk: {
            break :blk (if (checkOperands(expr.operator, l, r, .num)) |o| .{
                .num = o.l + o.r,
            } else if (checkOperands(expr.operator, l, r, .str)) |o| .{
                .str = try mem.concat(self.allocator, u8, &[_][]const u8{ o.l, o.r }),
            } else error.OperandsNeitherNumbersNorStrings);
        },
        .slash => if (opsn) |o| .{ .num = o.l / o.r } else error.OperandsNotNumbers,
        .star => if (opsn) |o| .{ .num = o.l * o.r } else error.OperandsNotNumbers,
        else => unreachable,
    };
}

fn visitLogicalExpr(self: *Self, expr: Expr.Logical) Error!Value {
    const l = try self.evaluate(expr.left);

    if (expr.operator.type == .@"or") {
        if (isTruthy(l)) return l;
    } else {
        if (!isTruthy(l)) return l;
    }

    return try self.evaluate(expr.right);
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

fn visitUnaryExpr(self: *Self, expr: Expr.Unary) Error!Value {
    const r = try self.evaluate(expr.right);

    return switch (expr.operator.type) {
        .minus => .{
            .num = -(checkOperand(expr.operator, r, .num) orelse
                return error.OperandNotNumber),
        },
        .bang => .{ .bool = !isTruthy(r) },
        else => unreachable,
    };
}

/// Lox follows Ruby’s simple rule: `false` and `nil` are falsey, and
/// everything else is truthy.
fn isTruthy(value: Value) bool {
    return switch (value) {
        .nil => false,
        .bool => |x| x,
        else => true,
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
        .bool => |x| std.fmt.format(writer, "{}", .{x}) catch {},
        .nil => std.fmt.format(writer, "nil", .{}) catch {},
        .num => |x| std.fmt.format(writer, "{d}", .{x}) catch {},
        // .obj => |x| std.fmt.format(writer, "{any}", .{x}) catch {},
        .str => |x| std.fmt.format(writer, "{s}", .{x}) catch {},
        .callable => |x| std.fmt.format(writer, "{s}", .{x.toString()}) catch {},
        .function => |x| std.fmt.format(writer, "{s}", .{x.toString()}) catch {},
        .ret => |x| std.fmt.format(writer, "{any}", .{x.toValue()}) catch {},
    }
}

/// Visit methods do a **post-order traversal**—each node evaluates its
/// children before doing its own work.
fn evaluate(self: *Self, expr: *Expr) Error!Value {
    return switch (expr.*) {
        .assign => |assign| blk: {
            const value = try self.evaluate(assign.value);
            self.environment.assign(assign.name, value) catch |err| {
                undeclared_token = assign.name;
                break :blk err;
            };
            break :blk value;
        },
        .binary => |binary| try self.visitBinaryExpr(binary),
        .call => |call| blk: {
            // Each call needs a new environment for recursion and concurrency.
            // call() creates environment, binds parameters, and executes.
            const callee = try self.evaluate(call.callee);
            const prev_environment = self.environment;
            defer self.environment = prev_environment;
            var environment = Environment.initEnclosing(self.allocator, prev_environment);
            self.environment = &environment;
            var arguments_list = try std.ArrayList(Value).initCapacity(self.allocator, call.arguments.len);
            errdefer arguments_list.deinit();
            for (call.arguments) |argument| {
                try arguments_list.append(try self.evaluate(argument));
            }
            const arguments: []Value = try arguments_list.toOwnedSlice();
            switch (callee) {
                .callable => |callable| { // native lox callable
                    if (arguments.len != callable.arity()) {
                        runtime_token = call.paren;
                        break :blk error.WrongArity;
                    }
                    break :blk callable.callFn(self, arguments);
                },
                .function => |function| { // lox function
                    if (arguments.len != function.arity()) {
                        runtime_token = call.paren;
                        break :blk error.WrongArity;
                    }
                    break :blk function.call(self, arguments);
                },
                else => {
                    runtime_token = call.paren;
                    break :blk error.NotCallable;
                },
            }
        },
        .grouping => |grouping| try self.evaluate(grouping),
        .literal => |literal| switch (literal) {
            .str => |str| .{ .str = try self.allocator.dupe(u8, str) },
            else => literal,
        },
        .logical => |logical| try self.visitLogicalExpr(logical),
        .unary => |unary| try self.visitUnaryExpr(unary),
        .variable => |variable| blk: {
            break :blk self.environment.get(variable) catch |err| {
                undeclared_token = variable;
                break :blk err;
            };
        },
    };
}

// See https://craftinginterpreters.com/statements-and-state.html#block-syntax-and-semantics
//
/// To execute a block, we create a new environment for the block’s scope
/// and pass it off to this other method:
//
// Manually changing and restoring a mutable environment field feels inelegant.
// Another classic approach is to explicitly pass the environment as a
// parameter to each visit method. To “change” the environment, you pass a
// different one as you recurse down the tree. You don’t have to restore the
// old one, since the new one lives on the Java stack and is implicitly
// discarded when the interpreter returns from the block’s visit method.
//
// I considered that for jlox, but it’s kind of tedious and verbose adding an
// environment parameter to every single visit method. To keep the book a
// little simpler, I went with the mutable field.
pub const EnvClosure = union(enum) {
    existing: *Environment,
    new: void,

    pub const Void = {};
};

/// Internally discards the block-local environment and restores the previous
/// one that was active back at the callsite.
// See https://craftinginterpreters.com/functions.html#function-objects
pub fn executeBlock(self: *Self, body: []Stmt, closure: EnvClosure, writer: anytype) Error!void {
    const prev_env = self.environment;
    defer self.environment = prev_env;

    switch (closure) {
        .existing => |env| self.environment = env,
        .new => {
            var curr_env = Environment.initEnclosing(self.allocator, prev_env);
            defer curr_env.deinit();
            self.environment = &curr_env;
        },
    }

    for (body) |*statement| {
        _ = try self.execute(statement, writer);
    }
}

pub fn execute(self: *Self, stmt: *Stmt, writer: anytype) Error!Value {
    switch (stmt.*) { // Visit .****Stmt
        .block => |statements| {
            _ = try self.executeBlock(statements, .{ .new = EnvClosure.Void }, writer);
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
        .function => |function| {
            // * When we create a LoxFunction, we capture the current environment.
            // * This is the environment that is active when the function is declared
            //   not when it’s called, which is what we want. It represents the
            //   lexical scope surrounding the function declaration. Finally, when we
            //   call the function, we use that environment as the call’s parent
            //   instead of going straight to globals.
            const fun = try makeLoxFunction(self.allocator, function, self.environment);
            try self.environment.define(function.name.lexeme, .{ .function = fun });
        },
        .print_stmt => |expr| {
            printValue(writer, try self.evaluate(expr));
            writer.writeByte('\n') catch return error.IoError; // multi-line
        },
        .return_stmt => |return_stmt| {
            // If we have a return value, we evaluate it, otherwise, we use nil.
            // Then we take that value and wrap it in a custom exception class and
            // throw it. We want this to unwind all the way to where the function
            // call began, the call() method in LoxFunction.
            const value: ?Value = if (return_stmt.value) |val| try self.evaluate(val) else null;
            runtime_token = return_stmt.keyword;
            runtime_return_value = Expr.LoxReturnValue.fromValue(value).ret;
            runtime_error = error.Return;
            return error.Return;
        },
        .var_stmt => |var_stmt| {
            const value = if (var_stmt.initializer) |expr| try self.evaluate(expr) else Value.Nil;
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

pub fn interpretExpression(self: *Self, expr: *Expr, writer: anytype) Allocator.Error!void {
    const value = self.evaluate(expr) catch |err| return handleRuntimeError(err);
    printValue(writer, value);
}

pub fn interpret(self: *Self, stmts: []Stmt, writer: anytype) Allocator.Error!void {
    if (comptime debug.is_trace_interpreter) {
        assert(self.environment.values.count() == 0);

        if (self.globals.values.get("clock")) |value| {
            const fun = value.callable;
            assert(@TypeOf(fun) == *LoxCallable);
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
            root.tracesrc(@src(), "output: '{any}'", .{value});
        }
    }
}
