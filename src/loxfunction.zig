//! Implements `LoxFunction`.

const std = @import("std");
const assert = std.debug.assert;
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

pub const FunctionContext = struct {
    allocator: Allocator,
    closure: *Environment,
    declaration: Stmt.Function,

    pub fn handleRuntimeError(self: *FunctionContext, err: Interpreter.Error) void {
        root.eprint("Error in function context: '{s}': ", .{self.declaration.name.lexeme});
        Interpreter.handleRuntimeError(err) catch |e| {
            root.exit(@intFromEnum(ErrorCode.runtime_error), "{any}.", .{e});
        };
    }
};

fn arityCtxFn(context: *anyopaque) usize {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    return ctx.declaration.parameters.len;
}

fn callCtxFn(context: *anyopaque, interpreter: *Interpreter, arguments: []Value) Value {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));

    for (ctx.declaration.parameters, 0..) |param, i| {
        interpreter.environment.define(param.lexeme, arguments[i]) catch |err| {
            ctx.handleRuntimeError(err);
            return Value.Nil;
        };
    }
    assert((if (Interpreter.runtime_error != undefined)
        Interpreter.runtime_token.type == .@"return"
    else
        Interpreter.runtime_error == undefined));

    const result: Value = interpreter.execute(
        @constCast(&Stmt{ .block = ctx.declaration.body }),
        root.stdout().writer(),
    ) catch |err|
        switch (err) {
        error.Return => blk: {
            assert(Interpreter.runtime_error == error.Return and Interpreter.runtime_token.type == .@"return");
            defer { // Reset thread-local variables runtime state captured on error
                Interpreter.runtime_error = undefined;
                Interpreter.runtime_return_value = undefined;
                Interpreter.runtime_token = undefined;
            }
            break :blk Interpreter.runtime_return_value;
        },
        else => blk: {
            ctx.handleRuntimeError(err);
            break :blk Value.Nil;
        },
    };

    return result;
}

fn toStringCtxFn(context: *anyopaque) []const u8 {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    const buf = std.fmt.allocPrint(ctx.allocator, "<fn {s}>", .{ctx.declaration.name.lexeme}) catch |err| { // See also https://stackoverflow.com/a/66665672
        root.exit(70, "{any}: toStringCtxFn: {any}\n", .{ ErrorCode.runtime_error, err });
    };
    errdefer ctx.allocator.free(buf);

    return buf;
}

pub fn functionCtxCallable(
    allocator: Allocator,
    declaration: Stmt.Function,
    closure: *Environment,
) Allocator.Error!*LoxFunction {
    const context = try allocator.create(FunctionContext);
    errdefer allocator.destroy(context);
    context.* = .{
        .allocator = allocator,
        .closure = closure,
        .declaration = declaration,
    };

    const out = try allocator.create(LoxFunction);
    errdefer allocator.destroy(out);
    out.* = LoxFunction{
        .context = context, //> `*Context` casted to `*anyopaque`

        .arityFn = arityCtxFn,
        .callFn = callCtxFn,
        .toStringFn = toStringCtxFn,
    };

    return out;
}
