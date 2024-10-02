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

comptime {
    assert(@sizeOf(@This()) == 80);
    assert(@alignOf(@This()) == 8);
}

pub fn init(
    allocator: Allocator,
    name: Token,
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

pub fn toString(callable: *const Callable) []const u8 {
    const self: *Class = @constCast(@fieldParentPtr("callable", callable));
    const name: []const u8 = self.name.lexeme;
    // const buffer: []u8 = try std.fmt.allocPrint(
    //     callable.allocator,
    //     "{s}", // "<class {s}>",
    //     .{self.name.lexeme},
    // );

    return name;
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
    try testing.expectEqual(80, @sizeOf(@This()));
    try testing.expectEqual(8, @alignOf(@This()));
}

test "Class initialization" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const class_declaration = Stmt.Class{
        .name = Token{ .type = .identifier, .lexeme = "TestClass", .line = 1, .literal = null },
        .methods = &[_]Stmt.Function{},
    };

    const cls = try Class.init(allocator, class_declaration.name);

    try testing.expectEqual(cls.name, class_declaration.name);
    try testing.expectEqualStrings("TestClass", cls.name.lexeme);
}

test "Class toString" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const class_declaration = Stmt.Class{
        .name = Token{ .type = .identifier, .lexeme = "TestClass", .line = 1, .literal = null },
        .methods = &[_]Stmt.Function{},
    };

    // /home/lloyd/p/lang_zig/crafting-interpreters-zig/src/function.zig:163:25: 0x1060e0b in test.Function toString (test)
    //     defer allocator.free(result);
    const cls = try Class.init(allocator, class_declaration.name);

    const result = cls.callable.toString();

    try testing.expectEqualStrings("TestClass", result);
}

test "Class arity" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const class_declaration = Stmt.Class{
        .name = Token{ .type = .identifier, .lexeme = "TestClass", .line = 1, .literal = null },
        .methods = &[_]Stmt.Function{},
    };

    const cls = try Class.init(allocator, class_declaration.name);

    try testing.expectEqual(@as(usize, 0), cls.callable.arity());
}
