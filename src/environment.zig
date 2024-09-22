const std = @import("std");
const root = @import("root.zig");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;
const StringHashMap = std.StringHashMap;

const Expr = @import("expr.zig").Expr;
const Token = @import("token.zig");

pub const Environment = struct {
    allocator: Allocator,
    enclosing: ?*Environment = null,
    values: StringHashMap(Expr.Value),

    const Self = @This();

    // note: `pub` required for union in `Interpreter.Error` due to `Environment.assign`.
    pub const Error = error{VariableNotDeclared} || Allocator.Error;

    // STUB
    // const GetOrPutResult = StringHashMap(Expr.Value).GetOrPutResult;

    pub fn init(allocator: Allocator) Allocator.Error!*Environment {
        const self = try allocator.create(Environment);

        self.* = .{
            .allocator = allocator,
            .values = StringHashMap(Expr.Value).init(allocator),
        };

        return self; // return try allocator.create(Environment).*.init(allocator);
    }

    /// TODO: Assert that enclosing is initialized
    ///
    // When a local variable has the same name as  a variable in an enclosing
    // scope, it shadows the outer one. Code inside the block can't see it any
    // more—it is hidden in the "shadow" cas by the inner one—but it's still there.
    // `pub`: Required by `Interpreter.execute() -> .block => |statements|`
    pub fn initEnclosing(enclosing: *Environment, allocator: Allocator) Environment {
        return .{
            .allocator = allocator,
            .enclosing = enclosing,
            .values = StringHashMap(Expr.Value).init(allocator),
        };
    }

    pub fn deinit() void {}

    /// NOTE: This function call clobbers values.
    pub fn define(self: *Self, name: []const u8, value: Expr.Value) Error!void {
        assert(name.len > 0 and root.isAlphaNumeric(name[0]));

        try self.values.put(name, value);
    }

    /// Get the value of a variable by its token name (`Token.lexeme`). It’s OK
    /// to refer to a variable before it’s defined as long as the reference is not
    /// evaluated.
    ///
    // Defers error to runtime (hard to define recursive functions if
    // mentioning a variable before declaration is a static error.)
    //
    // Scope and environments are close cousins. The former is the theoretical
    // concept, and the latter is the machinery that implements it.
    //
    // The beginning of a block introduces a new local scope, and that scope
    // ends when execution passes the closing }. Any variables declared inside
    // the block disappear.
    pub fn get(self: *const Self, name: Token) Error!Expr.Value {
        assert(name.lexeme.len > 0 and root.isAlphaNumeric(name.lexeme[0]));

        if (self.values.get(name.lexeme)) |value| return value;
        if (self.enclosing) |enclosing| return enclosing.get(name);

        return error.VariableNotDeclared;
    }

    /// Assign a new value to an existing variable.
    /// Note: Also no implicit var declaration.
    /// See also: https://craftinginterpreters.com/statements-and-state.html#design-note
    pub fn assign(self: *Self, name: Token, value: Expr.Value) Error!void {
        assert(name.lexeme.len > 0 and root.isAlphaNumeric(name.lexeme[0]));

        if (self.values.getPtr(name.lexeme)) |ptr| {
            ptr.* = value;
        } else if (self.enclosing) |enclosing| {
            // recursively find and assign variable in nearest enclosing
            try enclosing.assign(name, value);
        } else {
            return error.VariableNotDeclared;
        }
    }
};

test "define and get" { // $ zig test src/environment.zig 2>&1 | head
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const env = try Environment.init(arena.allocator());

    const name = "x";
    const value: Expr.Value = .{ .str = "hello" };
    try env.define(name, value);

    var tok: Token = .{ .type = .identifier, .lexeme = name, .literal = .{ .str = value.str }, .line = 1 };
    try testing.expectEqualStrings(value.str, (try env.get(tok)).str);

    tok.lexeme = "y";
    try testing.expectError(error.VariableNotDeclared, env.get(tok)); //catch |err| err
}

test "assign" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const env = try Environment.init(arena.allocator());

    const name = "x";
    const value: Expr.Value = .{ .str = "hello" };
    try env.define(name, value);

    var tok = Token{ .type = .identifier, .lexeme = name, .literal = .{ .str = value.str }, .line = 1 };
    try testing.expectEqualStrings(value.str, (try env.get(tok)).str);

    const new_value: Expr.Value = .{ .str = "world" };
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
    const value: Expr.Value = .{ .str = "hello" };
    try env.define(name, value);

    const tok1 = Token{ .type = .identifier, .lexeme = name, .literal = .{ .str = value.str }, .line = 1 };
    const tok2 = Token{ .type = .identifier, .lexeme = name, .literal = .{ .str = "not the original value" }, .line = 1 };
    try testing.expect(!mem.eql(u8, tok1.literal.?.str, tok2.literal.?.str)); // sanity check
    try testing.expectEqual(value, try env.get(tok2)); // Undefined Behavior since tok2 has different literal value, but same lexeme as tok1

    const new_value = Expr.Value{ .str = "world" };
    try testing.expect(!mem.eql(u8, new_value.str, (try env.get(tok1)).str));
}
