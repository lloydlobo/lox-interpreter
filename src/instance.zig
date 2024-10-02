//! `Instance` extends the runtime representation of an instance of a `Class`.

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const Callable = @import("callable.zig");
const Environment = @import("environment.zig");
const Expr = @import("expr.zig").Expr;
const Interpreter = @import("interpreter.zig");
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const Value = @import("value.zig").Value;
const logger = @import("logger.zig");
const root = @import("root.zig");
const Class = @import("class.zig");

const Instance = @This();

callable: Callable, // provides `VTable`
class: *Class,

comptime {
    // assert(@sizeOf(@This()) == 120);
    assert(@alignOf(@This()) == 8);
}

pub fn init(
    allocator: Allocator,
    class: *Class,
    // closure: *Environment,
    // declaration: Stmt.Function,
) Allocator.Error!*Instance {
    const self = try allocator.create(Instance);
    self.* = .{
        .callable = .{
            .allocator = allocator,
            .vtable = &vtable,
        },
        .class = class,
        // .closure = closure,
        // .declaration = declaration,
    };

    return self;
}

/// `self` should be the return value of `create`, or otherwise
/// have the same address and alignment property.
pub fn destroy(self: *Instance, allocator: Allocator) void {
    // self.callable.destroy(allocator);
    allocator.destroy(self);
}

pub const AllocPrintError = error{OutOfMemory};
pub const Error = AllocPrintError;

const vtable = Callable.VTable{
    .toString = toString,
    .call = call,
    .arity = arity,
};

pub fn toString(callable: *const Callable) AllocPrintError![]const u8 {
    const self: *Instance = @constCast(@fieldParentPtr("callable", callable));
    const buffer: []u8 = try std.fmt.allocPrint(callable.allocator, "{s} instance", .{
        self.class.name.lexeme,
    });

    return buffer;
}

pub fn call(callable: *const Callable, interpreter: *Interpreter, arguments: []Value) Callable.Error!Value {
    _ = arguments; // autofix
    _ = interpreter; // autofix

    const self: *Instance = @constCast(@fieldParentPtr("callable", callable));
    _ = self; // autofix

    @panic("Unimplemented"); // return Value.Nil;
}

pub fn arity(callable: *const Callable) usize {
    const self: *Instance = @constCast(@fieldParentPtr("callable", callable));

    const class_arity = self.class.callable.arity();
    assert(class_arity == 0);

    return 0; // TODO: Change this when we add arguments later on methods.
}

test "stats" {
    try testing.expectEqual(120, @sizeOf(@This()));
    try testing.expectEqual(8, @alignOf(@This()));
}

test "Instance initialization" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const env = try Environment.init(allocator);
    const declaration = Stmt.Function{
        .name = Token{ .type = .identifier, .lexeme = "testFunc", .line = 1, .literal = null },
        .parameters = &[_]Token{},
        .body = &[_]Stmt{},
    };

    const func = try Instance.init(allocator, env, declaration);
    defer func.destroy(allocator);

    try testing.expect(func.closure == env);
    try testing.expectEqualStrings("testFunc", func.declaration.name.lexeme);
}

test "Instance toString" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const env = try Environment.init(allocator);
    const declaration = Stmt.Function{
        .name = Token{ .type = .identifier, .lexeme = "testFunc", .line = 1, .literal = null },
        .parameters = &[_]Token{},
        .body = &[_]Stmt{},
    };

    const func = try Instance.init(allocator, env, declaration);
    defer func.destroy(allocator);

    const result = try func.callable.toString();
    defer allocator.free(result);

    try testing.expectEqualStrings("testFunc", result);
}

test "Instance arity" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const env = try Environment.init(allocator);
    const declaration = Stmt.Function{
        .name = Token{ .type = .identifier, .lexeme = "testFunc", .line = 1, .literal = null },
        .parameters = @constCast(&[_]Token{
            Token{ .type = .identifier, .lexeme = "a", .line = 1, .literal = null },
            Token{ .type = .identifier, .lexeme = "b", .line = 1, .literal = null },
        }),
        .body = &[_]Stmt{},
    };

    const func = try Instance.init(allocator, env, declaration);
    defer func.destroy(allocator);

    try testing.expectEqual(@as(usize, 2), func.callable.arity());
}
