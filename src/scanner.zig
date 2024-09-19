const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const root = @import("root.zig");
const mem = std.mem;
const Allocator = mem.Allocator;

const @"error" = @import("main.zig").@"error";
const Token = @import("token.zig").Token;

// See also https://craftinginterpreters.com/scanning.html#recognizing-lexemes

pub const Scanner = struct {
    source: []const u8,
    tokens: std.ArrayList(Token),

    start: usize = 0,
    current: usize = 0,
    line: u32 = 1,

    pub const keywords = std.StaticStringMap(Token.Type).initComptime(.{
        .{ "and", .@"and" },
        .{ "break", .@"break" },
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
        return if ((self.current + 1) >= self.source.len)
            0
        else
            self.source[self.current + 1];
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
