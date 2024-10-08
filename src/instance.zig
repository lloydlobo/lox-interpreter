//! `Instance` extends the runtime representation of an instance of a `Class`.

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const Callable = @import("callable.zig");
const Class = @import("class.zig");
const Environment = @import("environment.zig");
const Function = @import("function.zig");
const Interpreter = @import("interpreter.zig");
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const Value = @import("value.zig").Value;
const debug = @import("debug.zig");
const logger = @import("logger.zig");
const root = @import("root.zig");

const Instance = @This();

/// Callable provides `VTable`.
callable: Callable, // 24
class: *Class, // 8
fields: std.StringHashMap(Value), // 40

comptime {
    // assert(@sizeOf(@This()) == 72);
    assert(@alignOf(@This()) == 8);
}

pub const Error = Callable.Error;

const vtable = Callable.VTable{
    .toString = toString,
    .call = call,
    .arity = arity,
};

pub fn init(
    allocator: Allocator,
    class: *Class,
) Allocator.Error!*Instance {
    const self = try allocator.create(Instance);
    self.* = .{
        .callable = .{
            .allocator = allocator,
            .vtable = &vtable,
        },
        .class = class,
        .fields = std.StringHashMap(Value).init(allocator),
    };

    return self;
}

/// `self` should be the return value of `create`, or otherwise
/// have the same address and alignment property.
pub fn destroy(self: *Instance, allocator: Allocator) void {
    allocator.destroy(self);
}

/// Throws runtime error and exits if `fields` does not contain `name.lexeme`.
/// See ast node: `Expr.Get`.
/// Caller should handle errors and set runtime token for logging error.
pub fn get(self: *Instance, name: Token) !Value {
    if (self.fields.get(name.lexeme)) |value| {
        // Unreleated to returning dummy value like `nil` in other interpreted languages.
        if (comptime debug.is_trace_interpreter) {
            switch (value) {
                .nil => logger.info(.default, @src(), "Got nil value for '{}'", .{name}),
                else => {},
            }
        }
        return value;
    }

    const is_prev_stable_impl = false;
    if (is_prev_stable_impl) {
        if (self.class.find_method(name.lexeme)) |*method| {
            const fun = try self.callable.allocator.create(Function);
            fun.* = method.*;
            errdefer fun.destroy(self.callable.allocator);

            const value: Value = .{ .function = fun };
            return value;
        }
    } else {
        // NOTE: Related to [issue: #5](https://github.com/lloydlobo/crafting-interpreters-zig/issues/5)?
        //
        // NOTE: the resolver has a new __scope__ for `this`, so the interpreter
        // needs to create a corresponding environment for it. (resolver's
        // scope chains and interpreter's link environment must always be in
        // sync with each other).
        if (self.class.find_method(name.lexeme)) |*method| {
            const function: *Function = @constCast(method);
            return .{ .function = try function.bind(self) };
        }
    }

    return Interpreter.Error.undefined_property;
}

pub fn set(self: *Instance, name: Token, value: Value) Instance.Error!void {
    // Since Lox allows freely creating new fields on instances,
    // there’s no need to see if the key is already present.
    try self.fields.put(name.lexeme, value);
}

pub fn toString(callable: *const Callable) []const u8 {
    const self: *Instance = @constCast(@fieldParentPtr("callable", callable));

    const token: Token = self.class.name;
    const buffer = std.fmt.allocPrint(
        callable.allocator,
        "{s} instance",
        .{token.lexeme},
    ) catch |err| {
        Interpreter.panicRuntimeError(err, token);
    };

    return buffer;
}

pub fn call(
    callable: *const Callable,
    interpreter: *Interpreter,
    arguments: []Value,
) Instance.Error!Value {
    _ = arguments; // autofix
    _ = interpreter; // autofix

    const self: *Instance = @constCast(
        @fieldParentPtr("callable", callable),
    );
    _ = self; // autofix

    @panic("Unimplemented"); // return Value.Nil;
}

pub fn arity(callable: *const Callable) usize {
    const self: *Instance = @constCast(@fieldParentPtr("callable", callable));
    assert(self.class.callable.arity() == 0); // sanity check

    return 0; // TODO: Change this when we add arguments later on methods.
}

test "stats" {
    try testing.expectEqual(72, @sizeOf(@This()));
    try testing.expectEqual(8, @alignOf(@This()));
}

test "Instance initialization" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const env = try Environment.init(allocator);
    _ = env; // autofix
    const cls_declaration = Stmt.Class{
        .name = Token{ .type = .identifier, .lexeme = "TestClass", .line = 1, .literal = null },
        .methods = &[_]Stmt.Function{},
    };

    const fun_declaration = Stmt.Function{
        .name = Token{
            .type = .identifier,
            .lexeme = "testFunc",
            .line = 1,
            .literal = null,
        },
        .parameters = &[_]Token{},
        .body = &[_]Stmt{},
    };
    _ = fun_declaration; // autofix

    const cls = try Class.init(allocator, cls_declaration.name);
    defer cls.destroy(allocator);

    const instance = try Instance.init(allocator, cls);
    defer instance.destroy(allocator);

    // try testing.expect(instance.closure == env);
    try testing.expectEqualStrings("TestClass", instance.class.name.lexeme);
}

test "Instance toString" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const method_declaration =
        Stmt.Function{
        .name = Token{
            .type = .identifier,
            .lexeme = "test_class_instance_method_function",
            .line = 1,
            .literal = null,
        },
        .parameters = @constCast(
            &[_]Token{
                Token{ .type = .identifier, .lexeme = "a", .line = 1, .literal = null },
                Token{ .type = .identifier, .lexeme = "b", .line = 1, .literal = null },
            },
        ),
        .body = &[_]Stmt{},
    };

    var methods = std.ArrayList(Stmt.Function).init(allocator);
    try methods.append(method_declaration);

    const cls_declaration = Stmt.Class{
        .name = Token{ .type = .identifier, .lexeme = "TestClass", .line = 1, .literal = null },
        .methods = try methods.toOwnedSlice(),
    };

    try testing.expectEqualStrings(
        "test_class_instance_method_function",
        cls_declaration.methods[0].name.lexeme,
    );

    const cls = try Class.init(allocator, cls_declaration.name);
    defer cls.destroy(allocator);

    const instance = try Instance.init(allocator, cls);

    try testing.expectEqualStrings("TestClass", instance.class.name.lexeme);

    const result = instance.callable.toString();

    try testing.expectEqualStrings("TestClass instance", result);
}

test "Instance arity" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const env = try Environment.init(allocator);
    _ = env; // autofix
    const cls_declaration = Stmt.Class{
        .name = Token{ .type = .identifier, .lexeme = "TestClass", .line = 1, .literal = null },
        .methods = &[_]Stmt.Function{},
    };

    const fun_declaration = Stmt.Function{
        .name = Token{
            .type = .identifier,
            .lexeme = "testFunc",
            .line = 1,
            .literal = null,
        },
        .parameters = &[_]Token{},
        .body = &[_]Stmt{},
    };
    _ = fun_declaration; // autofix

    const cls = try Class.init(allocator, cls_declaration.name);
    defer cls.destroy(allocator);

    const instance = try Instance.init(allocator, cls);
    defer instance.destroy(allocator);

    try testing.expectEqual(@as(usize, 0), instance.callable.arity());
}
