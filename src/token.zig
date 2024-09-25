const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;

const formatNumber = @import("root.zig").formatNumber;

const Token = @This();

// pub const Token: type = struct {
lexeme: []const u8,
literal: ?Literal,
type: Type,
line: u32,

pub const Type = enum {
    // Single-character tokens

    /// `!`  33
    bang,
    /// `(`  40
    left_paren,
    /// `)`  41
    right_paren,
    /// `*`  42
    star,
    /// `+`  43
    plus,
    /// `,`  44
    comma,
    /// `-`  45
    minus,
    /// `.`  46
    dot,
    /// `/`  47
    slash,
    /// `;`  59
    semicolon,
    /// `<`  60
    less,
    /// `=`  61
    equal,
    /// `>`  62
    greater,
    /// `{`  123
    left_brace,
    /// `}`  125
    right_brace,

    // Multi-character tokens

    /// `==`
    equal_equal,
    /// `!=`
    bang_equal,
    /// `<=`
    less_equal,
    /// `>=`
    greater_equal,

    // Literals and identifiers

    /// `"`
    string,
    /// e.g. `foo` in `var foo;`
    identifier, // any lexeme starting with a letter or underscore
    /// `'0'...'9'`
    number,

    // Keywords

    @"and",
    @"break",
    class,
    @"else",
    false,
    @"for",
    fun,
    @"if",
    nil,
    @"or",
    print,
    @"return",
    super,
    this,
    true,
    @"var",
    @"while",

    //
    // whitespace,
    //
    // comment,
    ///      10
    eof,
};

pub const Literal = union(enum) {
    str: []const u8,
    num: f64,

    pub fn format(self: Literal, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .str => |str| try std.fmt.format(writer, "{s}", .{str}),
            .num => |num| try formatNumber(writer, num),
        }
    }
};

pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    var buf: [32]u8 = undefined;
    try std.fmt.format(writer, "{s} {s} {?}", .{
        std.ascii.upperString(&buf, @tagName(self.type)),
        self.lexeme,
        self.literal,
    });
}
// };

// copied from lib/std/multi_array_list.zig
test "basic usage with MultiArrayList" {
    const ally = testing.allocator;
    var list = std.MultiArrayList(Token){};
    defer list.deinit(ally);

    try testing.expectEqual(@as(usize, 0), list.items(.lexeme).len);

    try list.ensureTotalCapacity(ally, 2);

    list.appendAssumeCapacity(.{
        .literal = .{ .str = "hello" },
        .type = .identifier,
        .line = 1,
        .lexeme = "a",
    });
    list.appendAssumeCapacity(.{
        .literal = .{ .str = "world" },
        .type = .identifier,
        .line = 2,
        .lexeme = "b",
    });

    try testing.expectEqualSlices(u32, list.items(.line), &[_]u32{ 1, 2 });
    try testing.expectEqualSlices([]const u8, list.items(.lexeme), &[_][]const u8{ "a", "b" });
    try testing.expectEqualSlices(?Token.Literal, list.items(.literal), &[_]?Token.Literal{ .{ .str = "hello" }, .{ .str = "world" } });

    try testing.expectEqual(@as(usize, 2), list.items(.lexeme).len);
    try testing.expectEqualStrings("a", list.items(.lexeme)[0]);
    try testing.expectEqualStrings("b", list.items(.lexeme)[1]);
    try testing.expectEqual(@as(usize, 2), list.items(.literal).len);
    try testing.expectEqualStrings("hello", list.items(.literal)[0].?.str);
    try testing.expectEqualStrings("world", list.items(.literal)[1].?.str);

    list.appendAssumeCapacity(.{
        .literal = .{ .str = "!" },
        .type = .identifier,
        .line = 3,
        .lexeme = "c",
    });

    try testing.expectEqualSlices(u32, list.items(.line), &[_]u32{ 1, 2, 3 });
    try testing.expectEqualSlices([]const u8, list.items(.lexeme), &[_][]const u8{ "a", "b", "c" });
    try testing.expectEqualSlices(?Token.Literal, list.items(.literal), &[_]?Token.Literal{ .{ .str = "hello" }, .{ .str = "world" }, .{ .str = "!" } });

    try testing.expectEqual(@as(usize, 3), list.items(.lexeme).len);
    try testing.expectEqualStrings("a", list.items(.lexeme)[0]);
    try testing.expectEqualStrings("b", list.items(.lexeme)[1]);
    try testing.expectEqualStrings("c", list.items(.lexeme)[2]);
    try testing.expectEqual(@as(usize, 3), list.items(.literal).len);
    try testing.expectEqualStrings("hello", list.items(.literal)[0].?.str);
    try testing.expectEqualStrings("world", list.items(.literal)[1].?.str);
    try testing.expectEqualStrings("!", list.items(.literal)[2].?.str);

    // Add 6 more items to force a capacity increase.
    const variables = &[_][]const u8{ "a", "b", "c", "d", "e", "f", "h", "i", "j" };
    const assigned_var_count = list.items(.lexeme).len;
    assert(variables.len == (6 + assigned_var_count));
    var i: usize = 0;
    const line = list.items(.line).len;
    while (i < (variables.len - assigned_var_count)) : (i += 1) {
        try list.append(ally, .{
            .literal = .{ .str = "whatever" },
            .type = .identifier,
            .line = (@as(u32, @intCast(line)) + 1) + @as(u32, @intCast(i)),
            .lexeme = variables[assigned_var_count + i],
        });
    }

    try testing.expectEqualSlices(
        u32,
        &[_]u32{ 1, 2, 3, 4, 5, 6, 7, 8, 9 },
        list.items(.line),
    );
    try testing.expectEqualSlices(
        []const u8,
        &[_][]const u8{ "a", "b", "c", "d", "e", "f", "h", "i", "j" },
        list.items(.lexeme),
    );

    list.shrinkAndFree(ally, 3);

    try testing.expectEqualSlices(u32, list.items(.line), &[_]u32{ 1, 2, 3 });
    try testing.expectEqualSlices([]const u8, list.items(.lexeme), &[_][]const u8{ "a", "b", "c" });
    try testing.expectEqualSlices(?Token.Literal, list.items(.literal), &[_]?Token.Literal{ .{ .str = "hello" }, .{ .str = "world" }, .{ .str = "!" } });

    try testing.expectEqual(@as(usize, 3), list.items(.lexeme).len);
    try testing.expectEqualStrings("a", list.items(.lexeme)[0]);
    try testing.expectEqualStrings("b", list.items(.lexeme)[1]);
    try testing.expectEqualStrings("c", list.items(.lexeme)[2]);
    try testing.expectEqual(@as(usize, 3), list.items(.literal).len);
    try testing.expectEqualStrings("hello", list.items(.literal)[0].?.str);
    try testing.expectEqualStrings("world", list.items(.literal)[1].?.str);
    try testing.expectEqualStrings("!", list.items(.literal)[2].?.str);

    list.set(try list.addOne(ally), .{
        .literal = .{ .str = "xnopyt" },
        .type = .identifier,
        .line = 4,
        .lexeme = "d",
    });

    try testing.expectEqualStrings("xnopyt", list.pop().literal.?.str);
    try testing.expectEqualStrings("c", if (list.popOrNull()) |elem| elem.lexeme else "");
    try testing.expectEqual(@as(u32, 2), list.pop().line);
    try testing.expectEqual("a", list.pop().lexeme);
    try testing.expectEqual(@as(?Token, null), list.popOrNull());
}
