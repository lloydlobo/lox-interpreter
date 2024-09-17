const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const root = @import("root.zig");
const mem = std.mem;
const Allocator = mem.Allocator;

const @"error" = @import("main.zig").@"error";
const Token = @import("token.zig").Token;

// see also https://craftinginterpreters.com/scanning.html#recognizing-lexemes

pub const Scanner = struct {
    source: []const u8,

    // A MultiArrayList stores a list of a struct or tagged union type.
    // Instead of storing a single list of items, MultiArrayList stores
    // separate lists for each field of the struct or lists of tags and bare
    // unions.
    // This allows for memory savings if the struct or union has padding, and
    // also improves cache usage if only some fields or just tags are needed
    // for a computation.  The primary API for accessing fields is the
    // `slice()` function, which computes the start pointers for the array of
    // each field.  From the slice you can call `.items(.<field_name>)` to
    // obtain a slice of field values.
    // For unions you can call `.items(.tags)` or `.items(.data)`.
    //
    //   pub fn MultiArrayList(comptime T: type) type
    tokens: std.ArrayList(Token),

    start: usize = 0,
    current: usize = 0,
    line: u32 = 1,

    pub const keywords = std.StaticStringMap(Token.Type).initComptime(.{
        .{ "and", .@"and" },
        .{ "class", .class },
        .{ "else", .@"else" },
        .{ "false", .false },
        .{ "for", .@"for" },
        .{ "fun", .fun },
        .{ "if", .@"if" },
        .{ "nil", .nil },
        .{ "or", .@"or" },
        .{ "print", .print },
        .{ "return", .@"return" },
        .{ "super", .super },
        .{ "this", .this },
        .{ "true", .true },
        .{ "var", .@"var" },
        .{ "while", .@"while" },
    });

    pub fn init(source: []const u8, allocator: Allocator) Scanner {
        return .{
            .source = source,
            .tokens = std.ArrayList(Token).init(allocator),
        };
    }

    pub fn deinit(self: *Scanner) void {
        self.tokens.deinit();
    }

    pub fn scanTokens(self: *Scanner) ![]const Token {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }
        try self.tokens.append(.{
            .type = .eof,
            .lexeme = "",
            .literal = null,
            .line = self.line,
        });

        return self.tokens.items;
    }

    fn scanToken(self: *Scanner) !void {
        const c: u8 = self.advance();
        switch (c) {
            '(' => try self.addToken(.left_paren), // 40
            ')' => try self.addToken(.right_paren), // 41
            '{' => try self.addToken(.left_brace), // 123
            '}' => try self.addToken(.right_brace), // 125

            '.' => try self.addToken(.dot), // 46
            ',' => try self.addToken(.comma), // 44
            ';' => try self.addToken(.semicolon), // 59

            '-' => try self.addToken(.minus), // 45
            '+' => try self.addToken(.plus), // 43
            '*' => try self.addToken(.star), // 42
            '/' => { //47
                if (self.match('/')) { // comment goes until line end
                    while ((self.peek() != '\n') and !self.isAtEnd())
                        _ = self.advance();
                } else {
                    try self.addToken(.slash);
                }
            },

            '!' => try self.addToken(if (self.match('=')) .bang_equal else .bang), // 33
            '=' => try self.addToken(if (self.match('=')) .equal_equal else .equal), // 61
            '<' => try self.addToken(if (self.match('=')) .less_equal else .less), // 60
            '>' => try self.addToken(if (self.match('=')) .greater_equal else .greater), // 62

            ' ', '\r', '\t' => {}, // ignore whitespace, go back to beginning of scan loop

            '"' => try self.string(),
            '\n' => self.line += 1, //10

            '0'...'9' => try self.number(), // is digit
            'a'...'z', 'A'...'Z', '_' => try self.identifier(), // first char either alphabet or _

            else => @"error"(self.line, "Unexpected character: {c}", .{c}),
        }
    }

    fn identifier(self: *Scanner) !void {
        while (root.isAlphaNumeric(self.peek()))
            _ = self.advance();

        const @"type" = if (keywords.get(self.source[self.start..self.current])) |@"type"|
            @"type"
        else
            .identifier;

        try self.addToken(@"type");
    }

    fn number(self: *Scanner) !void {
        while (std.ascii.isDigit(self.peek()))
            _ = self.advance();

        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            _ = self.advance(); //consume the `.`

            while (std.ascii.isDigit(self.peek()))
                _ = self.advance();
        }

        const num = try std.fmt.parseFloat(f64, self.source[self.start..self.current]);
        try self.addTokenValue(.number, .{ .num = num });
    }

    fn string(self: *Scanner) !void {
        while ((self.peek() != '"') and !self.isAtEnd()) {
            if (self.peek() == '\n')
                self.line += 1;
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            @"error"(self.line, "Unterminated string.", .{});
            return;
        }
        _ = self.advance(); // the closing `"`

        assert((self.start + 1) <= (self.current - 1));
        const str = self.source[self.start + 1 .. self.current - 1]; // literal
        try self.addTokenValue(.string, .{ .str = str }); // trim the surrounding quotes
    }

    /// Similar to a conditional lookahead `advance()`. We only consume the current character if it’s what we’re looking for.
    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd())
            return false;
        if ((self.source[self.current] != expected))
            return false;

        self.current += 1;

        return true;
    }

    fn peek(self: *Scanner) u8 {
        return if (self.isAtEnd()) 0 else self.source[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        // return if (self.isAtEnd()) 0 else self.source[self.current + 1];
        return if ((self.current + 1) >= self.source.len) 0 else self.source[self.current + 1];
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Scanner) u8 {
        defer self.current += 1;

        return self.source[self.current];
    }

    fn addToken(self: *Scanner, @"type": Token.Type) !void {
        try self.addTokenValue(@"type", null);
    }

    fn addTokenValue(self: *Scanner, @"type": Token.Type, value: ?Token.Literal) !void {
        assert(self.start <= self.current);
        assert(self.line >= 1); // sanity check
        try self.tokens.append(.{
            .type = @"type",
            .lexeme = self.source[self.start..self.current],
            .literal = value,
            .line = self.line,
        });
    }
};

test "if not using ArrayList(Token) -> basic usage: MultiArrayList(Token){}" { // copied from lib/std/multi_array_list.zig
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
    // std.debug.print("{any}\n", .{list.get(0)});

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
