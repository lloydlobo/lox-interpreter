//! `FunctionContext` implements `LoxFunction` with `makeLoxFunction(...)`.

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const Environment = @import("environment.zig");
const ErrorCode = @import("main.zig").ErrorCode;
const Expr = @import("expr.zig").Expr;
const Value = Expr.Value;
const LoxFunction = Value.LoxFunction;
const Interpreter = @import("interpreter.zig");
const Stmt = @import("stmt.zig").Stmt;
const root = @import("root.zig");
const runtimeError = @import("main.zig").runtimeError;

const FunctionContext = @This();

allocator: Allocator,
/// Environment at function declaration, representing its lexical scope.
closure: *Environment,
declaration: Stmt.Function,

pub fn handleRuntimeError(self: *FunctionContext, err: Interpreter.Error) void {
    root.eprint("Error in function: '{s}': ", .{self.declaration.name.lexeme});
    Interpreter.handleRuntimeError(err) catch |e| root.exit(@intFromEnum(ErrorCode.runtime_error), "{any}.", .{e});
}

pub fn handleRuntimeErrorAndExit(self: *FunctionContext, err: Interpreter.Error) noreturn {
    self.handleRuntimeError(err);
    root.exit(@intFromEnum(ErrorCode.runtime_error), "{any}", .{err});
}

pub fn makeLoxFunction(allocator: Allocator, declaration: Stmt.Function, closure: *Environment) Allocator.Error!*LoxFunction {
    const context = try allocator.create(FunctionContext);
    errdefer allocator.destroy(context);
    context.* = .{ // `this` or `self` of `FunctionContext`
        .allocator = allocator,
        .closure = closure,
        .declaration = declaration,
    };

    const out = try allocator.create(LoxFunction);
    errdefer allocator.destroy(out);
    out.* = LoxFunction{
        .context = context, // casted to `*anyopaque`
        .arityFn = arityFn,
        .callFn = callFn,
        .toStringFn = toStringFn,
    };

    return out;
}

fn arityFn(context: *anyopaque) usize {
    return @as(*FunctionContext, @alignCast(@ptrCast(context))).declaration.parameters.len;
}

fn callFn(context: *anyopaque, interpreter: *Interpreter, arguments: []Value) Value {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    var environment = Environment.init(ctx.allocator) catch |err| ctx.handleRuntimeErrorAndExit(err);
    errdefer ctx.allocator.destroy(environment);

    environment = ctx.closure;

    for (ctx.declaration.parameters, 0..) |param, i| {
        ctx.closure.define(param.lexeme, arguments[i]) catch |err| ctx.handleRuntimeErrorAndExit(err);
    }

    _ = interpreter.executeBlock(
        ctx.declaration.body,
        environment,
        root.stdout().writer(),
    ) catch |err| switch (err) {
        error.Return => {
            assert(Interpreter.runtime_error == error.Return and
                Interpreter.runtime_token.type == .@"return");
            defer {
                Interpreter.runtime_error = undefined;
                Interpreter.runtime_return_value = undefined;
                Interpreter.runtime_token = undefined;
            }
            return Interpreter.runtime_return_value;
        },
        else => {
            ctx.handleRuntimeError(err);
            return Value.Nil;
        },
    };

    return Value.Nil;
}

fn toStringFn(context: *anyopaque) []const u8 {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    const buffer: []u8 = std.fmt.allocPrint(
        ctx.allocator,
        "<fn {s}>",
        .{ctx.declaration.name.lexeme},
    ) catch |err| ctx.handleRuntimeErrorAndExit(err); // See also https://stackoverflow.com/a/66665672
    errdefer ctx.allocator.free(buffer);

    return buffer;
}
