const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const Environment = @import("environment.zig").Environment;
const Expr = @import("expr.zig").Expr;
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig").Token;

const runtimeError = @import("main.zig").runtimeError;
const ErrorCode = @import("main.zig").ErrorCode;

pub const Interpreter = struct {
    environment: *Environment,
    allocator: Allocator,

    const Self = @This();

    const RuntimeError = error{
        IoError,
        OperandNotNumber,
        OperandsNotNumbers,
        OperandsNeitherNumbersNorStrings,
    };
    pub const Error = Allocator.Error || Environment.Error || RuntimeError;

    var runtime_token: Token = undefined;
    var undeclared_token: Token = undefined;

    pub fn init(allocator: Allocator) Allocator.Error!Interpreter {
        return .{
            .environment = try Environment.init(allocator),
            .allocator = allocator,
        };
    }

    fn handleRuntimeError(err: Error) Allocator.Error!void {
        return switch (err) {
            // RuntimeError
            error.OperandNotNumber => runtimeError(runtime_token, "Operand must be a number.", .{}),
            error.OperandsNotNumbers => runtimeError(runtime_token, "Operands must be numbers.", .{}),
            error.OperandsNeitherNumbersNorStrings => runtimeError(runtime_token, "Operands must two numbers or two strings.", .{}),
            error.IoError => {
                std.debug.print("IoError: {any}.", .{err});
                @panic("IoError");
            },

            // Environment.Error
            error.VariableNotDeclared => runtimeError(undeclared_token, "Undefined variable '{s}'.", .{undeclared_token.lexeme}),

            // Allocator.Error
            else => |other| other,
        };
    }

    pub fn interpret(self: *Self, stmts: []Stmt, writer: anytype) Allocator.Error!void {
        for (stmts) |*stmt| {
            _ = self.execute(stmt, writer) catch |err|
                return handleRuntimeError(err);
        }
    }

    pub fn interpretExpression(self: *Self, expr: *Expr, writer: anytype) Allocator.Error!void {
        const value = self.evaluate(expr) catch |err| return handleRuntimeError(err);
        printValue(writer, value);
    }

    fn execute(self: *Self, stmt: *Stmt, writer: anytype) Error!Expr.Value {
        switch (stmt.*) {
            .break_stmt => |break_stmt| {
                // TODO: The syntax is a break keyword followed by a semicolon. It should
                // be a syntax error to have a break statement appear outside of any
                // enclosing loop.

                const previous = self.environment;
                defer self.environment = previous;

                // MAY CAUSE INFINITE LOOPS
                var environment: Environment = previous.initEnclosing();
                self.environment = &environment;

                if (break_stmt) |label| std.debug.print("[debug] break_stmt: {any}\n", .{label});

                @panic("Unimplemented");
            },
            .block => |statements| {
                const previous = self.environment;
                defer self.environment = previous; // finally { ... }
                var environment = previous.initEnclosing(); // try { ... }
                self.environment = &environment;
                for (statements) |*statement|
                    _ = try self.execute(statement, writer);
            },
            .if_stmt => |if_stmt| {
                _ = if (isTruthy(try self.evaluate(if_stmt.condition)))
                    try self.execute(if_stmt.then_branch, writer)
                else if (if_stmt.else_branch) |eb|
                    try self.execute(eb, writer);
            },
            .var_stmt => |var_stmt| {
                const value: Expr.Value = if (var_stmt.initializer) |expr| try self.evaluate(expr) else .{ .nil = {} };
                try self.environment.define(var_stmt.name.lexeme, value);
            },
            .while_stmt => |while_stmt| { // similar to If visit method
                while (isTruthy(try self.evaluate(while_stmt.condition)))
                    _ = try self.execute(while_stmt.body, writer);
            },
            .print => |expr| {
                printValue(writer, try self.evaluate(expr));
                writer.writeByte('\n') catch return error.IoError; // multi-line
            },
            .expr => |expr| {
                _ = try self.evaluate(expr);
            },
        }

        return .{ .nil = {} };
    }

    /// Visit methods do a **post-order traversal**—each node evaluates its
    /// children before doing its own work.
    fn evaluate(self: *Self, expr: *Expr) Error!Expr.Value {
        return switch (expr.*) {
            .assign => |assign| blk: {
                // Similar to variable declarations in `execute() => .var_stmt`:
                // Recursively evaluate "rhs" to get the value, then store it
                // in a named variable, via `assign()` instead of `define()`.
                // Key change: assignment not allowed to create a new variable.
                const value = try self.evaluate(assign.value);
                self.environment.assign(assign.name, value) catch |err| {
                    undeclared_token = assign.name;
                    break :blk err; // key doesn't already exist in environments's variable map
                };
                break :blk value;
            },
            .binary => |binary| try self.visitBinaryExpr(binary),
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

    fn visitBinaryExpr(self: *Self, expr: Expr.Binary) Error!Expr.Value {
        // see also https://craftinginterpreters.com/evaluating-expressions.html#evaluating-binary-operators
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
            .str => |x| std.fmt.format(writer, "{s}", .{x}) catch {},
        }
    }
};
