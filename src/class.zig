//! Where an instance stores state, the class stores behavior. `Instance` has
//! its map of fields, and `Class` gets a map of methods. Even though methods
//! are owned by the class, they are still accessed through instances of that
//! class.

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const Callable = @import("callable.zig");
const Environment = @import("environment.zig");
const Expr = @import("expr.zig").Expr;
const Function = @import("function.zig");
const Instance = @import("instance.zig");
const Interpreter = @import("interpreter.zig");
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const Value = @import("value.zig").Value;
const logger = @import("logger.zig");
const root = @import("root.zig");

const Class = @This();

callable: Callable, // provides `VTable`. @sizeOf 24
name: Token, // @sizeOf 56
methods: std.StringHashMap(Function), // @sizeOf 40

comptime {
    assert(@sizeOf(@This()) == 120);
    assert(@alignOf(@This()) == 8);
}

pub const Error = Allocator.Error;

const vtable = Callable.VTable{
    .toString = toString,
    .call = call,
    .arity = arity,
};

pub fn init(
    allocator: Allocator,
    name: Token,
    methods: std.StringHashMap(Function),
) Allocator.Error!*Class {
    const self = try allocator.create(Class);
    self.* = .{
        .callable = .{
            .allocator = allocator,
            .vtable = &vtable,
        },
        .name = name,
        .methods = methods,
    };

    return self;
}

/// `self` should be the return value of `create`, or otherwise
/// have the same address and alignment property.
pub fn destroy(self: *Class, allocator: Allocator) void {
    allocator.destroy(self);
}

pub fn find_method(self: *Class, name: []const u8) ?Function {
    const methods: std.StringHashMap(Function) = self.methods;

    if (methods.get(name)) |fun| {
        return fun;
    }

    return null;
}

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
    const self: *Class = @constCast(@fieldParentPtr("callable", callable));

    const instance = Instance.init(self.callable.allocator, self) catch |err| {
        Interpreter.runtime_token = self.name;
        try Interpreter.handleRuntimeError(err);
        return err;
    };

    // We can do almost everything with classes now, and as we near the end of the
    // chapter we find ourselves strangely "focused on a beginning".
    // Methods and fields let us encapsulate state and behavior together so
    // that an object always stays in a valid configuration.
    // But how do we ensure a brand new object starts in a good state?
    var initializer_fun: ?Function = self.find_method("init");
    if (initializer_fun) |*initializer| {
        const init_method: *Function = try initializer.bind(instance);
        _ = try init_method.callable.call(interpreter, arguments);
    }

    const out: Value = .{ .instance = instance };
    return out;
}

/// If there is an initializer, that method’s arity determines how many
/// arguments you must pass when you call the class itself.
/// We don’t require a class to define an initializer, though, as a
/// convenience. If you don’t have an initializer, the arity is still zero.
pub fn arity(callable: *const Callable) usize {
    const self: *Class = @constCast(@fieldParentPtr("callable", callable));
    const initializer: Function = self.find_method("init") orelse return 0;

    return initializer.callable.arity();
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
