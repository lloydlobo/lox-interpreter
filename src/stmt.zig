const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const fmt = std.fmt;
const FormatOptions = fmt.FormatOptions;

const Expr = @import("expr.zig").Expr;
const Token = @import("token.zig").Token;

pub const Stmt = union(enum) {
    block: []Stmt,
    if_stmt: IfStmt,
    var_stmt: VarStmt,
    print: *Expr,
    expr: *Expr,

    pub const IfStmt = struct {
        condition: *Expr,
        then_branch: *Stmt,
        else_branch: ?*Stmt,

        // see also https://craftinginterpreters.com/appendix-ii.html#if-statement
    };

    pub const VarStmt = struct {
        name: Token,
        initializer: ?*Expr,
    };
};

// pub fn format(
//     self: IfStmt,
//     comptime _: []const u8,
//     _: FormatOptions,
//     writer: anytype,
// ) !void {
//     try std.fmt.format(writer, "if |> condition |> {}\n", .{self.condition});
//     const tb = self.then_branch.*;
//     try std.fmt.format(writer, "then |> {}\n", .{tb});
//     if (self.else_branch) |else_branch| {
//         const eb = else_branch.*;
//         try std.fmt.format(writer, "else |> {}\n", .{eb});
//     }
// }

// pub fn format(
//     self: VarStmt,
//     comptime _: []const u8,
//     _: FormatOptions,
//     writer: anytype,
// ) !void {
//     try std.fmt.format(writer, "{}", .{self.name});
//     if (self.initializer) |init|
//         try std.fmt.format(writer, "{}", .{init});
// }
// pub fn format(
//     self: Stmt,
//     comptime _: []const u8,
//     _: FormatOptions,
//     writer: anytype,
// ) !void {
//     switch (self) {
//         .block => |statements| {
//             try std.fmt.format(writer, "`block:` {{\n", .{});
//             for (statements) |stmt| try std.fmt.format(writer, "{}\n", .{stmt});
//             try std.fmt.format(writer, "}}", .{});
//         },
//         .if_stmt => |if_stmt| try std.fmt.format(writer, "{}", .{if_stmt}),
//         .var_stmt => |var_stmt| try std.fmt.format(writer, "{}", .{var_stmt}),
//         .print => |expr| try std.fmt.format(writer, "`print:` {{ {} }}", .{expr}),
//         .expr => |expr| try std.fmt.format(writer, "{}", .{expr}),
//     }
// }
//

// Wed Sep 18 03:58:09 PM IST 2024
