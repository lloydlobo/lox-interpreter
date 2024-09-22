const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const debug = @import("debug.zig");
const root = @import("root.zig");

const Environment = @import("environment.zig");
const ErrorCode = @import("main.zig").ErrorCode;
const Expr = @import("expr.zig").Expr;
const Value = Expr.Value;
const LoxCallable = Value.LoxCallable;
const LoxFunction = Value.LoxFunction;
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const runtimeError = @import("main.zig").runtimeError;
const functionCtxCallable = @import("loxfunction.zig").functionCtxCallable;

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

const Interpreter = @This();

allocator: Allocator,

/// Holds a fixed reference to the outermost global environment.
globals: *Environment,

/// Tracks current frame and changes as we enter and exit local scopes.
environment: *Environment,

const Self = @This();

const RuntimeError = error{
    IoError,
    OperandNotNumber,
    OperandsNotNumbers,
    OperandsNeitherNumbersNorStrings,
    WrongArity,
    NotCallable,
};
pub const Error = Allocator.Error || Environment.Error || RuntimeError;

var runtime_token: Token = undefined;
var undeclared_token: Token = undefined;

pub fn init(allocator: Allocator) Allocator.Error!Interpreter {
    const self: Interpreter = .{
        .allocator = allocator,
        .environment = try Environment.init(allocator),
        .globals = try Environment.init(allocator),
    };

    const clock_callable = try self.allocator.create(LoxCallable);
    clock_callable.* = clockGlobalCallable;

    self.globals.define("clock", .{ .callable = clock_callable }) catch |err|
        try handleRuntimeError(err);

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
        else => |other| other,
    };
}

fn visitBinaryExpr(self: *Self, expr: Expr.Binary) Error!Expr.Value {
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

fn visitLogicalExpr(self: *Self, expr: Expr.Logical) Error!Expr.Value {
    const l = try self.evaluate(expr.left);

    if (expr.operator.type == .@"or") {
        if (isTruthy(l)) return l;
    } else {
        if (!isTruthy(l)) return l;
    }

    return try self.evaluate(expr.right);
}

fn visitUnaryExpr(self: *Self, expr: Expr.Unary) Error!Expr.Value {
    const r = try self.evaluate(expr.right);

    return switch (expr.operator.type) {
        .minus => .{ .num = -(checkOperand(expr.operator, r, .num) orelse return error.OperandNotNumber) },
        .bang => .{ .bool = !isTruthy(r) },
        else => unreachable,
    };
}

/// Matches operator type with kind and returns its type.
fn checkOperand(
    operator: Token,
    operand: Expr.Value,
    comptime kind: std.meta.FieldEnum(Expr.Value),
) ?std.meta.fieldInfo(Expr.Value, kind).type {
    return switch (operand) {
        kind => |x| x,
        else => {
            runtime_token = operator;
            return null;
        },
    };
}

fn checkOperands(
    operator: Token,
    left: Expr.Value,
    right: Expr.Value,
    comptime kind: std.meta.FieldEnum(Expr.Value),
) ?struct {
    l: std.meta.fieldInfo(Expr.Value, kind).type,
    r: std.meta.fieldInfo(Expr.Value, kind).type,
} {
    return .{
        .l = checkOperand(operator, left, kind) orelse return null,
        .r = checkOperand(operator, right, kind) orelse return null,
    };
}

/// Lox follows Ruby’s simple rule: `false` and `nil` are falsey, and
/// everything else is truthy.
fn isTruthy(value: Expr.Value) bool {
    return switch (value) {
        .nil => false,
        .bool => |x| x,
        else => true,
    };
}

fn isEqual(a: Expr.Value, b: Expr.Value) bool {
    switch (a) {
        .str => |l| switch (b) {
            .str => |r| return mem.eql(u8, l, r),
            else => {},
        },
        else => {},
    }

    return std.meta.eql(a, b);
}

fn printValue(writer: anytype, value: Expr.Value) void {
    switch (value) {
        .bool => |x| std.fmt.format(writer, "{}", .{x}) catch {},
        .nil => std.fmt.format(writer, "nil", .{}) catch {},
        .num => |x| std.fmt.format(writer, "{d}", .{x}) catch {},
        .obj => |x| std.fmt.format(writer, "{any}", .{x}) catch {},
        .str => |x| std.fmt.format(writer, "{s}", .{x}) catch {},
        .callable => |x| std.fmt.format(writer, "{s}", .{x.toString()}) catch {},
        .function => |x| std.fmt.format(writer, "{s}", .{x.toString()}) catch {},
    }
}

pub fn execute(self: *Self, stmt: *Stmt, writer: anytype) Error!Expr.Value {
    root.log_fn("execute", "{}", .{stmt}, writer);

    switch (stmt.*) {
        .block => |statements| {
            // Discard that block-local environment and restores the
            // previous one that was active back at the callsite.
            const previous = self.environment;
            defer self.environment = previous;
            var environment = Environment.initEnclosing(previous, self.allocator);
            self.environment = &environment;

            for (statements) |*statement|
                _ = try self.execute(statement, writer);
        },
        .break_stmt => |break_stmt| {
            _ = break_stmt;
            @panic("Unimplemented");
        },
        .expr => |expr| {
            _ = try self.evaluate(expr);
        },
        .if_stmt => |if_stmt| {
            _ = if (isTruthy(try self.evaluate(if_stmt.condition)))
                try self.execute(if_stmt.then_branch, writer)
            else if (if_stmt.else_branch) |eb|
                try self.execute(eb, writer);
        },
        .function => |fun| {
            const res = try functionCtxCallable(self.allocator, fun); // A new env was created that is local to function call site and current is defer assigned back after it returns.
            // std.log.debug("here in .function -> res: {any}", .{res});
            try self.environment.define(fun.name.lexeme, .{ .function = res });
        },
        .print => |expr| {
            // std.log.debug("here in .print -> res: {any}", .{expr});
            printValue(writer, try self.evaluate(expr));
            writer.writeByte('\n') catch return error.IoError; // multi-line
        },
        .var_stmt => |var_stmt| {
            const value = if (var_stmt.initializer) |expr| try self.evaluate(expr) else Value.Nil;
            try self.environment.define(var_stmt.name.lexeme, value);
        },
        .while_stmt => |while_stmt| { // similar to If visit method
            while (isTruthy(try self.evaluate(while_stmt.condition))) {
                _ = try self.execute(while_stmt.body, writer);
            }
        },
    }

    return Value.Nil;
}

/// Visit methods do a **post-order traversal**—each node evaluates its
/// children before doing its own work.
fn evaluate(self: *Self, expr: *Expr) Error!Expr.Value {
    root.log_fn("evaluate", "{}", .{expr}, std.io.getStdOut().writer());

    return switch (expr.*) {
        // Similar to variable declarations in `execute() => .var_stmt`:
        // Key change: assignment not allowed to create a new variable.
        .assign => |assign| blk: {
            const value = try self.evaluate(assign.value); // Recursively evaluate "rhs" -> get value, and store in named variable
            self.environment.assign(assign.name, value) catch |err| {
                undeclared_token = assign.name;
                break :blk err; // key doesn't already exist in environments's variable map
            };
            break :blk value;
        },
        .binary => |binary| try self.visitBinaryExpr(binary),

        // First, we evaluate the expression for the callee. Typically, this
        // expression is just an identifier that looks up the function by its
        // name, but it could be anything. Then we evaluate each of the
        // argument expressions in order and store the resulting values in a
        // list.
        //
        // This is another one of those subtle semantic choices. Since argument
        // expressions may have side effects, the order they are evaluated
        // could be user visible. Even so, some languages like Scheme and C
        // don’t specify an order. This gives compilers freedom to reorder them
        // for efficiency, but means users may be unpleasantly surprised if
        // arguments aren’t evaluated in the order they expect.
        //
        // Further, this environment must be created dynamically. Each function call
        // gets its own environment. Otherwise, recursion would break. If there are
        // multiple calls to the same function in play at the same time, each needs its
        // own environment, even though they are all calls to the same function.
        //
        // That’s why we create a new environment at each call, not at the function
        // declaration. The call() method we saw earlier does that. At the beginning of
        // the call, it creates a new environment. Then it walks the parameter and
        // argument lists in lockstep. For each pair, it creates a new variable with
        // the parameter’s name and binds it to the argument’s value.
        .call => |call| blk: {
            const callee = try self.evaluate(call.callee);

            // Discards the function-local environment and restores the previous one
            // that was active back at the callsite.
            const previous_environment = self.environment;
            defer self.environment = previous_environment;
            var environment = Environment.initEnclosing(previous_environment, self.allocator);
            errdefer environment.deinit(); // Unimplemented
            self.environment = &environment;

            var arguments = std.ArrayList(Expr.Value).init(self.allocator);
            errdefer arguments.deinit();

            for (call.arguments) |argument| {
                try arguments.append(try self.evaluate(argument));
            }

            // FIXME: I think we need to return functions rather than calling then in evaluate!
            const args = try arguments.toOwnedSlice();
            switch (callee) {
                .callable => |callable| { // native fn
                    if (args.len != callable.arity()) {
                        runtime_token = call.paren;
                        break :blk error.WrongArity;
                    }
                    break :blk callable.callFn(self, args);
                },
                .function => |function| { // user fn
                    if (args.len != function.arity()) {
                        runtime_token = call.paren;
                        break :blk error.WrongArity;
                    }
                    // self.context when?
                    //      LoxCallable function = (LoxCallable)callee;
                    //      return function.call(this, arguments);
                    break :blk function.call(self, args);
                },
                else => {
                    runtime_token = call.paren;
                    break :blk error.NotCallable;
                },
            }
            unreachable; // just in case
        },
        .grouping => |group| try self.evaluate(group),
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
        // else => @panic("Unimplemented"),
    };
}

pub fn interpretExpression(self: *Self, expr: *Expr, writer: anytype) Allocator.Error!void {
    const value = self.evaluate(expr) catch |err| return handleRuntimeError(err);
    printValue(writer, value);
}

pub fn interpret(self: *Self, stmts: []Stmt, writer: anytype) Allocator.Error!void {
    if (comptime debug.is_trace_env) {
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
        if (self.environment.values.count() != 0) {
            self.environment.values.clearAndFree();
        }
        assert(self.environment.values.count() == 0);
    }

    for (stmts) |*stmt| {
        _ = self.execute(stmt, writer) catch |err|
            return handleRuntimeError(err);
    }
}
