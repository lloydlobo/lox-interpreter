const std = @import("std");

const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const Interpreter = @import("../interpreter.zig");
const Value = @import("../value.zig").Value;
const Callable = @import("../callable.zig");

const builtin = @This();

pub const default_vtable: *const Callable.VTable = &.{
    .toString = struct {
        fn toString(_: *const Callable) Callable.AllocPrintError![]const u8 {
            return "<native fn>";
        }
    }.toString,
    .call = struct {
        pub fn call(_: *const Callable, _: *Interpreter, _: []Value) Callable.Error!Value {
            return Value.Nil;
        }
    }.call,
    .arity = struct {
        fn arity(_: *const Callable) usize {
            return 0;
        }
    }.arity,
};

pub const clock_vtable: *const Callable.VTable = &.{
    .toString = struct {
        fn toString(self: *const Callable) Callable.AllocPrintError![]const u8 {
            _ = self; // autofix
            return "<native fn>";
        }
    }.toString,

    .call = struct {
        pub fn call(_: *const Callable, _: *Interpreter, _: []Value) Callable.Error!Value {
            return .{ .num = (@as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0) };
        }
    }.call,

    .arity = struct {
        fn arity(self: *const Callable) usize {
            _ = self; // autofix
            return 0;
        }
    }.arity,
};
