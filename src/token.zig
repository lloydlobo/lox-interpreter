const std = @import("std");

const formatNumber = @import("root.zig").formatNumber;

pub const Token: type = struct {
    type: Type,
    lexeme: []const u8,
    literal: ?Literal,
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
};
