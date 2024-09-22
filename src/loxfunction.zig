//! Implements `LoxFunction`.

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const ErrorCode = @import("main.zig").ErrorCode;
const Expr = @import("expr.zig").Expr;
const Value = Expr.Value;
const LoxFunction = Value.LoxFunction;
const Interpreter = @import("interpreter.zig");
const Environment = @import("environment.zig");
const handleRuntimeError = Interpreter.handleRuntimeError;
const runtimeError = @import("main.zig").runtimeError;
const root = @import("root.zig");
const Stmt = @import("stmt.zig").Stmt;

// pub const FunctionContext = struct {
//     allocator: Allocator,
//     declaration: Stmt.Function,
// };
pub const FunctionContext = struct {
    allocator: Allocator,
    declaration: Stmt.Function,
};

fn arityCtxFn(context: *anyopaque) usize {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    return ctx.declaration.parameters.len;
}

fn callCtxFn(context: *anyopaque, interpreter: *Interpreter, arguments: []Value) Value {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    var block = Stmt{ .block = ctx.declaration.body };
    const params = ctx.declaration.parameters;

    for (params, 0..) |param, i| {
        const argument = arguments[i];
        interpreter.environment.define(param.lexeme, argument) catch |err| {
            root.eprint("Error in function: '{s}': ", .{ctx.declaration.name.lexeme});
            handleRuntimeError(err) catch |e|
                root.exit(@intFromEnum(ErrorCode.runtime_error), "Failed to call handleRuntimeError: {any}.", .{e});
            return Value.Nil;
        };
    }

    const value = interpreter.execute(&block, root.stdout().writer()) catch |err| {
        root.eprint("Error in function: '{s}': ", .{ctx.declaration.name.lexeme});
        handleRuntimeError(err) catch |e|
            root.exit(@intFromEnum(ErrorCode.runtime_error), "Failed to call handleRuntimeError: {any}.", .{e});
        return Value.Nil;
    };

    const __return_enabled = false;
    if (__return_enabled) {
        return value;
    } else {
        return Value.Nil;
    }
}

fn toStringCtxFn(context: *anyopaque) []const u8 {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    const buf = std.fmt.allocPrint(ctx.allocator, "<fn {s}>", .{ctx.declaration.name.lexeme}) catch |err| { // See also https://stackoverflow.com/a/66665672
        root.exit(70, "{any}: toStringCtxFn: {any}\n", .{ ErrorCode.runtime_error, err });
    };
    errdefer ctx.allocator.free(buf);

    return buf;
}

pub fn functionCtxCallable(allocator: Allocator, declaration: Stmt.Function) Allocator.Error!*LoxFunction {
    const context = try allocator.create(FunctionContext);
    errdefer allocator.destroy(context);
    context.* = .{
        .allocator = allocator,
        .declaration = declaration,
    };

    const out = try allocator.create(LoxFunction);
    errdefer allocator.destroy(out);
    out.* = LoxFunction{
        .arityFn = arityCtxFn,
        .callFn = callCtxFn,
        .toStringFn = toStringCtxFn,
        .context = context, //> `*Context` cast to `*anyopaque`
    };

    return out;
}
