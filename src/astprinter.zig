const std = @import("std");

const Expr = @import("expr.zig").Expr;
const Stmt = @import("stmt.zig").Stmt;

pub const AstPrinter = struct {
    pub fn print(writer: anytype, expr: *Expr) anyerror!void {
        try switch (expr.*) {
            .literal => |literal| std.fmt.format(writer, "{}", .{literal}),
            .unary => |unary| parenthesize(writer, unary.operator.lexeme, .{unary.right}),
            .binary => |binary| parenthesize(writer, binary.operator.lexeme, .{ binary.left, binary.right }),
            .grouping => |grouping| parenthesize(writer, "group", .{grouping}), //since lox uses `group` while printing
            .assign => {},
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

    // STUB
    // pub fn printStmt(writer: anytype, stmt: *Stmt) anyerror!void {
    //     switch (stmt.*) {
    //         .expr, .print => |x| try AstPrinter.print(writer, x),
    //         .if_stmt => |x| {
    //             try AstPrinter.print(writer, x.condition);
    //             try AstPrinter.printStmt(writer, x.then_branch);
    //             if (x.else_branch) |eb| {
    //                 try AstPrinter.printStmt(writer, eb);
    //             }
    //         },
    //         .block => |statements| {
    //             for (statements) |*x| {
    //                 try AstPrinter.printStmt(writer, x);
    //             }
    //         },
    //         else => |_| {},
    //     }
    // }
};
