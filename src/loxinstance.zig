//! `InstanceContext` implements `LoxInstance` (which extends the runtime
//! representation of an instance of a `LoxClass`) with `makeLoxInstance(...)`.

const std = @import("std");

const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const ClassContext = @import("loxclass.zig");
const Expr = @import("expr.zig").Expr;
const Token = @import("token.zig");
const Interpreter = @import("interpreter.zig");
const Value = @import("value.zig").Value;
const LoxClass = @import("value.zig").Value.LoxClass;
const LoxInstance = @import("value.zig").Value.LoxInstance;
const logger = @import("logger.zig");

const InstanceContext = @This();

allocator: Allocator,
class: *ClassContext,

comptime {
    // TODO:
    // assert(@sizeOf(@This()) == 112);
    assert(@alignOf(@This()) == 8);
}

pub fn createLoxInstance(allocator: Allocator, class: *ClassContext) Allocator.Error!*LoxInstance {
    const context = try allocator.create(InstanceContext); // should this be a closure?
    errdefer allocator.destroy(context);
    context.* = .{
        .allocator = allocator,
        .class = class, // parent context
    };

    // Usage: from interpreter.visitCallExpr(self: *Self, call: Expr.Call) Error!Value { ... }
    //
    // break :blk callable.callFn(self, try arguments.toOwnedSlice());
    const out = try allocator.create(LoxInstance);
    errdefer allocator.destroy(out);
    out.* = .{
        .context = context, // casted to `*anyopaque`

        // Implementing `LoxCallable` interface requires 3 methods:
        .toStringFn = toStringFn,
        .callFn = callFn,
        .arityFn = arityFn,

        .get = getFn,
    };

    return out;
}

pub fn getFn(context: *anyopaque, token: Token) Value {
    // HI!!!
    const ctx = @as(*InstanceContext, @alignCast(@ptrCast(context)));
    logger.info(.default, @src(), "In getFn: {s}{s}", .{
        toStringFn(ctx),
        ctx.class.name,
    });
    Interpreter.runtime_token = token;
    Interpreter.handleRuntimeError(error.non_instance_property) catch unreachable;

    // const buffer: []u8 = std.fmt.allocPrint(ctx.allocator, "{} instance", .{ctx.class.name});
    // return buffer;
    return Value.Nil;
}

pub fn toStringFn(context: *anyopaque) []const u8 {
    const ctx = @as(*InstanceContext, @alignCast(@ptrCast(context)));
    const buffer: []u8 = std.fmt.allocPrint(
        ctx.allocator,
        "{s} instance",
        .{ctx.class.name},
    ) catch |err| blk: {
        Interpreter.handleRuntimeError(err) catch |e| {
            std.log.err("{}", .{e});
            unreachable;
        };
        const msg = @src().fn_name ++ " unimplemented toStringFn";
        break :blk @constCast(msg);
    };
    return buffer;
}

fn callFn(context: *anyopaque, interpreter: *Interpreter, arguments: []Value) Value {
    const ctx = @as(*ClassContext, @alignCast(@ptrCast(context)));

    _ = ctx; // autofix
    _ = arguments; // autofix
    _ = interpreter; // autofix

    // TODO:
    @panic("Unimplemented"); // return Value.Nil;
}

fn arityFn(context: *anyopaque) usize {
    _ = context; // autofix
    // return @as(*ClassContext, @alignCast(@ptrCast(context))).declaration.parameters.len;
    return 0;
}

test "basic usage" {
    // try testing.expectEqual(0, @sizeOf(@This()));
    // try testing.expectEqual(1, @alignOf(@This()));
}

// https://craftinginterpreters.com/classes.html
