const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const Callable = @import("callable.zig");
const Instance = @import("instance.zig");
const Environment = @import("environment.zig");
const Expr = @import("expr.zig").Expr;
const Interpreter = @import("interpreter.zig");
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const Value = @import("value.zig").Value;
const logger = @import("logger.zig");
const root = @import("root.zig");

const Class = @This();

callable: Callable, // provides `VTable`
name: Token,
// closure: *Environment,
// declaration: Stmt.Class,

comptime {
    // assert(@sizeOf(@This()) == 120);
    assert(@alignOf(@This()) == 8);
}

pub fn init(
    allocator: Allocator,
    name: Token,

    // closure: *Environment,
    // declaration: Stmt.Class,
    //      name: Token,
    //      methods: []Stmt.Function,
) Allocator.Error!*Class {
    const self = try allocator.create(Class);
    self.* = .{
        .callable = .{
            .allocator = allocator,
            .vtable = &vtable,
        },
        .name = name,
        // .declaration = declaration,
    };

    return self;
}

/// `self` should be the return value of `create`, or otherwise
/// have the same address and alignment property.
pub fn destroy(self: *Class, allocator: Allocator) void {
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
    const self: *Class = @constCast(@fieldParentPtr("callable", callable));

    const buffer: []u8 = try std.fmt.allocPrint(
        callable.allocator,
        "<class {s}>",
        .{self.name.lexeme},
    );

    return buffer;
}

/// Instantiates a new `Instance` for the called class and returns it.
pub fn call(
    callable: *const Callable,
    interpreter: *Interpreter,
    arguments: []Value,
) Callable.Error!Value {
    _ = arguments; // autofix
    _ = interpreter; // autofix
    const self: *Class = @constCast(@fieldParentPtr("callable", callable));

    const instance = Instance.init(
        self.callable.allocator,
        self,
    ) catch |err| {
        Interpreter.runtime_token = self.name;
        try Interpreter.handleRuntimeError(err);
        return err;
    };

    const out: Value = .{ .instance = instance };
    return out;
}

/// NOTE: Bare class initializers for now, so arity is 0.
pub fn arity(callable: *const Callable) usize {
    const self: *Class = @constCast(@fieldParentPtr("callable", callable));
    _ = self; // autofix

    return 0;
}

test "stats" {
    try testing.expectEqual(120, @sizeOf(@This()));
    try testing.expectEqual(8, @alignOf(@This()));
}

test "Class initialization" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const env = try Environment.init(allocator);
    const declaration = Stmt.Class{
        .name = Token{ .type = .identifier, .lexeme = "testFunc", .line = 1, .literal = null },
        .parameters = &[_]Token{},
        .body = &[_]Stmt{},
    };

    const func = try Class.init(allocator, env, declaration);
    defer func.destroy(allocator);

    try testing.expect(func.closure == env);
    try testing.expectEqualStrings("testFunc", func.declaration.name.lexeme);
}

test "Class toString" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const env = try Environment.init(allocator);
    const declaration = Stmt.Class{
        .name = Token{ .type = .identifier, .lexeme = "testFunc", .line = 1, .literal = null },
        .parameters = &[_]Token{},
        .body = &[_]Stmt{},
    };

    const func = try Class.init(allocator, env, declaration);
    defer func.destroy(allocator);

    const result = try func.callable.toString();
    defer allocator.free(result);

    try testing.expectEqualStrings("testFunc", result);
}

test "Class arity" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const env = try Environment.init(allocator);
    const declaration = Stmt.Class{
        .name = Token{ .type = .identifier, .lexeme = "testFunc", .line = 1, .literal = null },
        .parameters = @constCast(&[_]Token{
            Token{ .type = .identifier, .lexeme = "a", .line = 1, .literal = null },
            Token{ .type = .identifier, .lexeme = "b", .line = 1, .literal = null },
        }),
        .body = &[_]Stmt{},
    };

    const func = try Class.init(allocator, env, declaration);
    defer func.destroy(allocator);

    try testing.expectEqual(@as(usize, 2), func.callable.arity());
}
