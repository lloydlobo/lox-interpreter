//! `ClassContext` implements `LoxClass` with `fn makeClassContext()`

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const Environment = @import("environment.zig");
const InstanceContext = @import("loxinstance.zig");
const Interpreter = @import("interpreter.zig");
const LoxClass = @import("expr.zig").Expr.Value.LoxClass;
const Value = @import("expr.zig").Expr.Value;
const root = @import("root.zig");
const main = @import("main.zig");

const ClassContext = @This();

allocator: Allocator,
name: []const u8,

comptime {
    // assert(@sizeOf(@This()) == 112);
    assert(@alignOf(@This()) == 8);
}

pub fn createLoxClass(allocator: Allocator, name: []const u8) Allocator.Error!*LoxClass {
    const context = try allocator.create(ClassContext);
    errdefer allocator.destroy(context);
    context.* = .{
        .allocator = allocator,
        .name = name,
    };

    const out = try allocator.create(LoxClass);
    errdefer allocator.destroy(out);
    out.* = .{
        .context = context, // casted to `*anyopaque`

        // Implementing `LoxCallable` interface requires 3 methods:
        .toStringFn = toStringFn,
        .callFn = callFn,
        .arityFn = arityFn,
    };

    return out;
}

fn handleRuntimeError(self: *ClassContext, err: Interpreter.Error) void {
    root.eprint("Error in function: '{s}': ", .{self.name});
    Interpreter.handleRuntimeError(err) catch |e|
        root.exit(.runtime_error, "{any}.", .{e});
}

fn handleRuntimeErrorAndExit(self: *ClassContext, err: Interpreter.Error) noreturn {
    self.handleRuntimeError(err);
    root.exit(.runtime_error, "{any}", .{err});
}

fn toStringFn(context: *anyopaque) []const u8 {
    const ctx = @as(*ClassContext, @alignCast(@ptrCast(context)));
    const name = ctx.name;

    const buffer = std.fmt.allocPrint(
        ctx.allocator,
        "<fn {s}>",
        .{name},
    ) catch |err| {
        ctx.handleRuntimeErrorAndExit(err); //> noreturn
    };
    errdefer ctx.allocator.free(buffer);
    comptime assert(@TypeOf(buffer) == []u8);

    return buffer;
}

/// Instantiates a new `LoxInstance` for the called class and returns it.
fn callFn(context: *anyopaque, interpreter: *Interpreter, arguments: []Value) Value {
    var ctx = @as(*ClassContext, @alignCast(@ptrCast(context)));
    _ = arguments; // autofix
    _ = interpreter; // autofix

    const instance: *Value.LoxInstance = InstanceContext.createLoxInstance(
        ctx.allocator,
        ctx, // *@This()
        // createLoxClass(ctx.allocator, ctx.name) catch |err| {
        //     ctx.handleRuntimeErrorAndExit(err);
        // },
    ) catch |err| {
        ctx.handleRuntimeErrorAndExit(err);
    };

    return .{ .instance = instance };
}

fn arityFn(context: *anyopaque) usize {
    _ = context; // autofix
    // NOTE: When we get to user-defined constructors, we’ll revisit this.
    //      return @as(*ClassContext, @alignCast(@ptrCast(context))).declaration.parameters.len;
    return 0;
}
