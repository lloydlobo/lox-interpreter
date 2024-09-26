const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;
const StringHashMap = std.StringHashMap;

const Token = @import("token.zig");
const Value = @import("expr.zig").Expr.Value;
const root = @import("root.zig");

const Environment = @This();

allocator: Allocator,
enclosing: ?*Environment = null,
values: StringHashMap(Value),
// values: std.AutoHashMap([]const u8, Value),

pub const Closure = union(enum) {
    existing: *Environment,
    new: void,

    pub const Void = {};
};

pub const Error = error{
    variable_not_declared,
} || Allocator.Error;

const Self = @This();

pub fn init(allocator: Allocator) Allocator.Error!*Environment {
    const self = try allocator.create(Environment);
    self.* = .{
        .allocator = allocator,
        .enclosing = null,
        .values = StringHashMap(Value).init(allocator),
    };

    return self;
}

pub fn deinit(self: *Environment) void {
    _ = self;
    std.log.warn("Tried to deinit unimplemented Environment.deinit()", .{});
}

pub fn initEnclosing(allocator: Allocator, enclosing: *Environment) Environment {
    return .{
        .allocator = allocator,
        .enclosing = enclosing,
        .values = StringHashMap(Value).init(allocator),
    };
}

pub fn define(self: *Self, name: []const u8, value: Value) Error!void {
    assert(name.len > 0 and root.isAlphaNumeric(name[0]));
    try self.values.put(name, value);
}

/// FIXME: this seems a bit buggy...
// pub fn ancestor(self: *Environment, distance: usize) *Environment {
pub fn ancestor(self: *@This(), distance: i32) *Environment {
    if (comptime false) {
        var environment = Environment.init(self.allocator) catch |err| {
            root.exit(.runtime_error, "{}", .{err});
        };
        defer self.allocator.destroy(environment);
        environment = @constCast(self); // avoid mutating original environment
    }

    var environment: *Environment = self; // Maybe it's time to use self: as @This()

    // Traverse up the chain for the specified depth:
    // * The interpreter code trusts that the resolver did its job and resolved the variable correctly.
    // * This implies a deep coupling between these two classes.
    // * In the resolver, each line of code that touches a scope must have its exact match in the interpreter for modifying an environment.
    var i: i32 = 0;
    while (i < distance) : (i += 1) {
        environment = environment.enclosing orelse unreachable;
    }

    return environment; // ancestor at specified depth (distance)
}

pub fn get(self: *const Self, name: Token) Error!Value {
    assert(name.lexeme.len > 0 and root.isAlphaNumeric(name.lexeme[0]));
    if (self.values.get(name.lexeme)) |value| {
        return value;
    }
    if (self.enclosing) |enclosing| {
        return enclosing.get(name);
    }

    return Error.variable_not_declared;
}

pub fn getAt(self: *Environment, distance: i32, name: Token) Error!Value {
    return try self.ancestor(distance).get(name);
}

pub fn assign(self: *Self, name: Token, value: Value) Error!void {
    assert(name.lexeme.len > 0 and root.isAlphaNumeric(name.lexeme[0]));

    if (self.values.getPtr(name.lexeme)) |ptr| {
        ptr.* = value;
    } else if (self.enclosing) |enclosing| {
        try enclosing.assign(name, value);
    } else {
        return Error.variable_not_declared;
    }
}

pub fn assignAt(self: *Environment, distance: i32, name: Token, value: Value) Error!void {
    try self.ancestor(distance).assign(name, value);
}

test "define and get" { // $ zig test src/environment.zig 2>&1 | head
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const env = try Environment.init(arena.allocator());

    const name = "x";
    const value: Value = .{ .str = "hello" };
    try env.define(name, value);

    var tok: Token = .{
        .type = .identifier,
        .lexeme = name,
        .literal = .{ .str = value.str },
        .line = 1,
    };
    try testing.expectEqualStrings(value.str, (try env.get(tok)).str);

    tok.lexeme = "y";
    try testing.expectError(Error.variable_not_declared, env.get(tok)); //catch |err| err
}

test "assign" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const env = try Environment.init(arena.allocator());

    const name = "x";
    const value: Value = .{ .str = "hello" };
    try env.define(name, value);

    var tok = Token{
        .type = .identifier,
        .lexeme = name,
        .literal = .{ .str = value.str },
        .line = 1,
    };
    try testing.expectEqualStrings(value.str, (try env.get(tok)).str);

    const new_value: Value = .{ .str = "world" };
    tok.literal.?.str = new_value.str;
    try testing.expect(!mem.eql(u8, value.str, new_value.str)); // sanity check
    try env.assign(tok, new_value);
    try testing.expectEqualStrings(new_value.str, (try env.get(tok)).str);
}

test "some errors and undefined behavior" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const env = try Environment.init(arena.allocator());

    const name = "x";
    const value: Value = .{ .str = "hello" };
    try env.define(name, value);

    const tok1 = Token{
        .type = .identifier,
        .lexeme = name,
        .literal = .{ .str = value.str },
        .line = 1,
    };
    const tok2 = Token{
        .type = .identifier,
        .lexeme = name,
        .literal = .{ .str = "not the original value" },
        .line = 1,
    };
    try testing.expect(!mem.eql(
        u8,
        tok1.literal.?.str,
        tok2.literal.?.str,
    )); // sanity check
    try testing.expectEqual(value, try env.get(tok2)); // undefined Behavior since tok2 has different literal value, but same lexeme as tok1

    const new_value = Value{ .str = "world" };
    try testing.expect(!mem.eql(
        u8,
        new_value.str,
        (try env.get(tok1)).str,
    ));
}

test "ancestor" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const global = try Environment.init(allocator);
    var local1 = Environment.initEnclosing(allocator, global);
    var local2 = Environment.initEnclosing(allocator, &local1);

    try testing.expectEqual(global, local2.ancestor(2));
    try testing.expectEqual(&local1, local2.ancestor(1));
    try testing.expectEqual(&local2, local2.ancestor(0));
}

test "getAt and assignAt" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const global = try Environment.init(allocator);
    var local1 = Environment.initEnclosing(allocator, global);
    var local2 = Environment.initEnclosing(allocator, &local1);

    const name = "x";
    const value: Value = .{ .str = "global" };
    try global.define(name, value);

    const local_value: Value = .{ .str = "local" };
    try local1.define(name, local_value);

    const tok: Token = .{
        .type = .identifier,
        .lexeme = name,
        .literal = .{ .str = value.str },
        .line = 1,
    };

    // Test `getAt`
    try testing.expectEqualStrings(value.str, (try local2.getAt(2, tok)).str);
    try testing.expectEqualStrings(local_value.str, (try local2.getAt(1, tok)).str);

    // Test `assignAt`
    const new_value: Value = .{ .str = "new global" };
    try local2.assignAt(2, tok, new_value);
    try testing.expectEqualStrings(new_value.str, (try local2.getAt(2, tok)).str);

    const new_local_value: Value = .{ .str = "new local" };
    try local2.assignAt(1, tok, new_local_value);
    try testing.expectEqualStrings(new_local_value.str, (try local2.getAt(1, tok)).str);
}

// ancestor()
// Walks a fixed number of hops up the parent chain and returns the
// `Environment` there.
//
// The `get()` method dynamically walks the chain of enclosing environments,
// scouring each one to see if the variable might be hiding in there
// somewhere. But now we know exactly which environment in the chain will have
// the variable. We reach it using this helper method:
// See also: const GetOrPutResult = StringHashMap(Value).GetOrPutResult;
//
// The interpreter code trusts that the resolver did its job and
// resolved the variable correctly. This implies a deep coupling
// between these two classes. In the resolver, each line of code that
// touches a scope must have its exact match in the interpreter for
// modifying an environment.

// initEnclosing()
// When a local variable has the same name as  a variable in an enclosing
// scope, it shadows the outer one. Code inside the block can't see it any
// more—it is hidden in the "shadow" cas by the inner one—but it's still there.

// Closure
// See https://craftinginterpreters.com/statements-and-state.html#block-syntax-and-semantics
//
// To execute a block, we create a new environment for the block’s scope
// and pass it off to this other method:
//
// Manually changing and restoring a mutable environment field feels inelegant.
// Another classic approach is to explicitly pass the environment as a
// parameter to each visit method. To “change” the environment, you pass a
// different one as you recurse down the tree. You don’t have to restore the
// old one, since the new one lives on the Java stack and is implicitly
// discarded when the interpreter returns from the block’s visit method.
//
// I considered that for jlox, but it’s kind of tedious and verbose adding an
// environment parameter to every single visit method. To keep the book a
// little simpler, I went with the mutable field.

// assign()
// Assign a new value to an existing variable.
// Note: Also no implicit var declaration.
// See also: https://craftinginterpreters.com/statements-and-state.html#design-note

// get()
// Get the value of a variable by its token name (`Token.lexeme`). It’s OK
// to refer to a variable before it’s defined as long as the reference is not
// evaluated.
//
// Defers error to runtime (hard to define recursive functions if
// mentioning a variable before declaration is a static error.)
// Scope and environments are close cousins. The former is the theoretical
// concept, and the latter is the machinery that implements it.
// The beginning of a block introduces a new local scope, and that scope
// ends when execution passes the closing }. Any variables declared inside
// the block disappear.

// assign()
// Assign a new value to an existing variable.
// Note: Also no implicit var declaration.
// See also: https://craftinginterpreters.com/statements-and-state.html#design-note
