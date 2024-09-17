const std = @import("std");
const root = @import("root.zig");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;
const StringHashMap = std.StringHashMap;

const Expr = @import("expr.zig").Expr;
const Token = @import("token.zig").Token;

const is_assert_enabled: bool = true;

pub const Environment = struct {
    values: StringHashMap(Expr.Value),
    enclosing: ?*Environment = null,

    const Self = @This();

    const EnvError = error{VariableNotDeclared};
    pub const Error = EnvError || Allocator.Error; // note: `pub` required for union in `Interpreter.Error` due to `Environment.assign`.

    const GetOrPutResult = StringHashMap(Expr.Value).GetOrPutResult;

    pub fn init(allocator: Allocator) Allocator.Error!*Environment {
        const self = try allocator.create(Environment);
        self.* = .{
            .values = StringHashMap(Expr.Value).init(allocator),
        };

        return self;
    }

    // When a local variable has the same name as  a variable in an enclosing
    // scope, it shadows the outer one. Code inside the block can't see it any
    // more—it is hidden in the "shadow" cas by the inner one—but it's still there.
    pub fn initEnclosing(enclosing: *Self, allocator: Allocator) Environment {
        // `pub`: Required by `Interpreter.execute() -> .block => |statements|`
        return .{
            .values = StringHashMap(Expr.Value).init(allocator),
            .enclosing = enclosing,
        };
    }

    pub fn deinit(self: *Self) void {
        var this = self;
        this.values.deinit();
    }

    /// Define (declare) a new variable by binding a name to a value.
    ///
    // if (false) {
    //     // TODO: hack to allow shadowing variables with `var`
    //     const res: GetOrPutResult = try self.values.getOrPut(name);
    //     if (res.found_existing) {
    //         try self.assign(.{ .literal = .{ .str = self.values.get(name).?.str }, .lexeme = name, .type = .identifier, .line = 1 }, value);
    //         // return error.VariableAlreadyDeclared; // do not clobber any existing data
    //     }
    //     res.value_ptr.* = value;
    // } else {
    //     ...
    // }
    pub fn define(self: *Self, name: []const u8, value: Expr.Value) Error!void {
        assert(name.len > 0 and root.isAlphaNumeric(name[0])); // expect token name start with alphanumeric

        try self.values.put(name, value); //clobbers values
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
        assert(name.lexeme.len > 0 and root.isAlphaNumeric(name.lexeme[0])); // expect token name start with alphanumeric

        assert(if (name.literal) |literal| switch (literal) {
            .str => |x| x.len > 0,
            .num => |x| @TypeOf(x) == f64,
        } else true);

        if (self.values.get(name.lexeme)) |value|
            return value;
        if (self.enclosing) |enclosing|
            return enclosing.get(name);

        return error.VariableNotDeclared;
    }

    /// Assign a new value to an existing variable.
    /// Note: Also no implicit var declaration. See also: https://craftinginterpreters.com/statements-and-state.html#design-note
    pub fn assign(self: *Self, name: Token, value: Expr.Value) Error!void {
        assert(name.lexeme.len > 0 and root.isAlphaNumeric(name.lexeme[0])); // expect token name start with alphanumeric

        assert(if (name.literal) |literal| switch (literal) {
            .str => |x| x.len > 0,
            .num => |x| @TypeOf(x) == f64,
        } else true);

        if (self.values.getPtr(name.lexeme)) |ptr| {
            ptr.* = value;
        } else if (self.enclosing) |enclosing| {
            try enclosing.assign(name, value); // recursively find var in the enclosing env
        } else {
            return error.VariableNotDeclared;
        }
    }
};

test "define and get" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const env = try Environment.init(allocator);
    defer env.deinit();

    // define: `var x = "hello";`
    const name = "x";
    const value: Expr.Value = .{ .str = "hello" };
    try env.define(name, value);

    // get: `x`
    var tok = Token{ .type = .identifier, .lexeme = name, .literal = .{ .str = value.str }, .line = 1 };
    const actual: Expr.Value = try env.get(tok);
    try testing.expectEqualStrings(value.str, actual.str);

    // get: `y`
    tok.lexeme = "y";
    try testing.expectError(error.VariableNotDeclared, env.get(tok) catch |err| err);
}

test "assign" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const env = try Environment.init(allocator);
    defer env.deinit();

    // define: `var x = "hello";`
    const name = "x";
    const value: Expr.Value = .{ .str = "hello" };
    try env.define(name, value);

    // get: `x`
    var tok = Token{ .type = .identifier, .lexeme = name, .literal = .{ .str = value.str }, .line = 1 };
    var actual: Expr.Value = try env.get(tok);
    try testing.expectEqualStrings(value.str, actual.str);

    const new_value: Expr.Value = .{ .str = "world" };
    tok.literal.?.str = new_value.str;
    try testing.expect(!mem.eql(u8, value.str, new_value.str));

    // assign `x = "world";`
    try env.assign(tok, new_value);
    actual = try env.get(tok);
    try testing.expectEqualStrings(new_value.str, actual.str);
}

test "some errors and undefined behavior" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const env = try Environment.init(allocator);
    defer env.deinit();

    // define: `var x = "hello";`
    const name = "x";
    const value = Expr.Value{ .str = "hello" };
    try env.define(name, value);

    const tok1 = Token{ .type = .identifier, .lexeme = name, .literal = .{ .str = value.str }, .line = 1 };
    const tok2 = Token{ .type = .identifier, .lexeme = name, .literal = .{ .str = "not the original value" }, .line = 1 };
    try testing.expect(!mem.eql(u8, tok1.literal.?.str, tok2.literal.?.str)); // sanity check

    // get: `x`
    try testing.expectEqual(value, try env.get(tok2)); // Undefined Behavior since tok2 has different literal value, but same lexeme as tok1

    // define: x="world"; // Undefined API usage
    const new_value = Expr.Value{ .str = "world" };
    // try testing.expectError(error.VariableAlreadyDeclared, env.define(name, new_value) catch |err| err);
    try testing.expect(!mem.eql(u8, new_value.str, (try env.get(tok1)).str));
}
