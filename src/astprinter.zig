const std = @import("std");

const Expr = @import("expr.zig").Expr;
const Stmt = @import("stmt.zig").Stmt;

pub const AstPrinter = struct {
    pub fn print(writer: anytype, expr: *Expr) anyerror!void {
        try switch (expr.*) {
            .assign => {},
            .binary => |binary| parenthesize(writer, binary.operator.lexeme, .{ binary.left, binary.right }),
            .grouping => |grouping| parenthesize(writer, "group", .{grouping}), // since lox uses `group` while printing
            .literal => |literal| std.fmt.format(writer, "{}", .{literal}),
            .unary => |unary| parenthesize(writer, unary.operator.lexeme, .{unary.right}),
            .variable => {},

            else => @panic("Unimplemented"),
        };
    }

    pub fn parenthesize(writer: anytype, name: []const u8, exprs: anytype) !void {
        try writer.print("({s}", .{name});

        inline for (std.meta.fields(@TypeOf(exprs))) |field| {
            const expr = @field(exprs, field.name);
            try writer.writeByte(' ');
            try print(writer, expr);
        }

        try writer.writeByte(')');
    }
};
