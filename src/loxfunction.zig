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

    if (Interpreter.runtime_error == error.Return) {
        root.tracesrc(@src(), "match runtime_error: '{any}', '{any}'=====", .{ Interpreter.runtime_error, arguments });
    }
    const writer = root.stdout().writer();
    const result: Value = interpreter.execute(&block, writer) catch |err| blk: {
        switch (err) {
            error.Return => |e| {
                const retval = Interpreter.runtime_return_value;
                root.tracesrc(@src(), "=====in result: '{any}', '{any}'=====", .{ e, retval });
                Interpreter.runtime_return_value = undefined; //RESET
                break :blk retval;
            },
            else => {
                root.eprint("Error in function: '{s}': ", .{ctx.declaration.name.lexeme});
                handleRuntimeError(err) catch |e| root.exit(@intFromEnum(ErrorCode.runtime_error), "Failed to call handleRuntimeError: {any}.", .{e});
                break :blk Value.Nil;
            },
        }
        unreachable;
    };

    // const ret: Value = switch (result) {
    //     .ret => |ret| blk: {
    //         std.log.debug("loxfunction.zig: in callCtxFn(): .ret => ret: {any}\n", .{ret});
    //         root.tracesrc(@src(), "=====.ret => ret: {any}", .{ret});
    //         break :blk ret.*.ret;
    //     },
    //     else => blk: {
    //         root.tracesrc(@src(), "=====else => result: {any}", .{result});
    //         break :blk Value.Nil;
    //     },
    // };
    root.tracesrc(@src(), "result: {any}, result {any}", .{ result, result });

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
