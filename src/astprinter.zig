const std = @import("std");
const assert = std.debug.assert;

const Expr = @import("expr.zig").Expr;
const Value = @import("value.zig").Value;
const Token = @import("token.zig");
const Stmt = @import("stmt.zig").Stmt;
const debug = @import("debug.zig");

const AstPrinter = @This();

comptime {
    assert(@sizeOf(@This()) == 0);
    assert(@alignOf(@This()) == 1);
}

pub fn print(writer: anytype, expr: *Expr) anyerror!void {
    try switch (expr.*) {
        .assign => {},
        .binary => |binary| parenthesize(writer, binary.operator.lexeme, .{ binary.left, binary.right }),
        .call => {},
        .get => {},
        .grouping => |grouping| parenthesize(writer, "group", .{grouping}), // since lox uses `group` while printing
        .literal => |literal| std.fmt.format(writer, "{}", .{literal}),
        .logical => {},
        .set => {},
        .this => {},
        .unary => |unary| parenthesize(writer, unary.operator.lexeme, .{unary.right}),
        .variable => {},
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

/// This is not to be used for command `tokenize` but just for debugging.
pub fn debugOtherVariants(writer: anytype, expr: *Expr) anyerror!void {
    switch (expr.*) {
        .assign => {},
        .binary => {},
        .call => |call| {
            // const callee: *Expr = call.callee;
            const name: Token =
                switch (call.callee.*) {
                .variable => |variable| variable,
                .get => |get| get.name,
                else => unreachable,
            };
            try writer.print("debug: ({s}", .{name.lexeme});

            try writer.print(" call(", .{});
            for (call.arguments, 0..) |argument, i| {
                try parenthesize(writer, "arg", .{argument});
                if ((i + 1) < call.arguments.len) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print("))\n", .{});
        },
        .get => {},
        .grouping => {},
        .literal => {},
        .logical => |logical| {
            try writer.print("debug: ", .{});
            try parenthesize(writer, logical.operator.lexeme, .{ logical.left, logical.right });
            try writer.print("\n", .{});
        },
        .set => {},
        .this => |this| try std.fmt.format(writer, "debug: ({any})\n", .{this.keyword}),
        .unary => {},
        .variable => {},
    }
}
