const std = @import("std");

const Token = @import("token.zig").Token;
const formatNumber = @import("root.zig").formatNumber;

pub const Expr = union(enum) {
    /// → NUMBER | STRING | "true" | "false" | "nil" ;
    literal: Value,
    /// → ( "-" | "!" ) expression ;
    unary: Unary,
    /// → expression operator expression ;
    binary: Binary,
    /// → "(" expression ")" ;
    grouping: *Expr,
    /// ?
    assign: Assign,
    /// ?
    variable: Token,

    pub const Assign = struct {
        name: Token,
        value: *Expr,
    };

    pub const Binary = struct {
        left: *Expr,
        /// → "==" | "!=" | "<" | "<=" | ">" | ">=" | "+"  | "-"  | "*" | "/" ;
        operator: Token,
        right: *Expr,
    };

    pub const Unary = struct {
        operator: Token,
        right: *Expr,
    };

    pub const Value = union(enum) {
        bool: bool,
        nil: void,
        num: f64,
        str: []const u8,

        pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .bool => |val| try std.fmt.format(writer, "{}", .{val}),
                .nil => try std.fmt.format(writer, "nil", .{}),
                .num => |val| try formatNumber(writer, val),
                .str => |val| try std.fmt.format(writer, "{s}", .{val}),
            }
        }
    };
};
