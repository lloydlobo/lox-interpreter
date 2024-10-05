const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const Expr = @import("expr.zig").Expr;
const Interpreter = @import("interpreter.zig");
const Token = @import("token.zig");
const Value = @import("value.zig").Value;
const formatNumber = @import("root.zig").formatNumber;
const logger = @import("logger.zig");

const loxbuiltin = @import("builtin/root.zig");
const loxcore = @import("core/root.zig");

const Callable = @This();

allocator: Allocator,
vtable: *const VTable,

comptime {
    assert(@sizeOf(@This()) == 24);
    assert(@alignOf(@This()) == 8);
}

pub const Error = Allocator.Error;

pub const VTable = struct {
    toString: *const fn (*const Callable) []const u8,
    call: *const fn (*const Callable, *Interpreter, []Value) Allocator.Error!Value,
    arity: *const fn (*const Callable) usize,
};

/// Returns a pointer to undefined memory.
/// Call `destroy` with the result to free the memory.
pub fn init(allocator: Allocator) Allocator.Error!*Callable {
    const out = try allocator.create(Callable);
    errdefer allocator.destroy(out);
    out.* = .{
        .allocator = allocator,
        .vtable = undefined,
    };

    return out;
}

/// `self` should be the return value of `create`, or otherwise
/// have the same address and alignment property.
pub fn destroy(self: *Callable, allocator: Allocator) void {
    allocator.destroy(self);
}

pub fn toString(self: *const Callable) []const u8 {
    return self.vtable.toString(self);
}

pub fn call(
    self: *const Callable,
    interpreter: *Interpreter,
    arguments: []Value,
) Callable.Error!Value {
    return try self.vtable.call(self, interpreter, arguments);
}

pub fn arity(self: *const Callable) usize {
    return self.vtable.arity(self);
}

test "stats" {
    try testing.expectEqual(24, @sizeOf(@This()));
    try testing.expectEqual(8, @alignOf(@This()));
}

test "Callable ─ native clock function" {
    const default_vtable = loxbuiltin.default_vtable;
    _ = default_vtable; // autofix
    const clock_vtable = loxbuiltin.clock_vtable;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const clock: Callable = .{
        .allocator = allocator,
        .vtable = clock_vtable,
    };

    var interpreter = try Interpreter.init(allocator);
    const arguments = &[_]Value{};

    try testing.expectEqualStrings("<native fn>", clock.toString());

    const actual: f64 = (try clock.call(&interpreter, arguments)).num;
    const expected = (@as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0);
    try testing.expectEqual(expected, actual);

    try testing.expectEqual(0, clock.arity());
}
