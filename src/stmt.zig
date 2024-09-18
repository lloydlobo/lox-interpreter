const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const fmt = std.fmt;
const FormatOptions = fmt.FormatOptions;

const Expr = @import("expr.zig").Expr;
const Token = @import("token.zig").Token;

pub const Stmt = union(enum) {
    block: []Stmt,
    expr: *Expr,
    if_stmt: If,
    print: *Expr,
    var_stmt: Var,
    while_stmt: While,

    // since else clauses are optional, and there is no explicit delimiter
    // marking the end of the if statement, the grammar is ambiguous when you
    // nest ifs in this way ─ This classic syntax pitfall is the [dangling else
    // problem](https://en.wikipedia.org/wiki/Dangling_else).
    // Solution: " `else` is bound to the nearest `if` that precedes it. "
    // See https://craftinginterpreters.com/appendix-ii.html#if-statement
    pub const If = struct {
        condition: *Expr,
        then_branch: *Stmt,
        else_branch: ?*Stmt,
    };

    pub const Var = struct {
        name: Token,
        initializer: ?*Expr,
    };

    pub const While = struct {
        condition: *Expr,
        body: *Stmt,
    };
};

// See https://craftinginterpreters.com/appendix-ii.html#statements
// A2.2 Statements
//
// Statements form a second hierarchy of syntax tree nodes independent of
// expressions. We add the first couple of them in “Statements and State”.
//
// package com.craftinginterpreters.lox;
//
// import java.util.List;
//
// abstract class Stmt {
//   interface Visitor<R> {
//     R visitBlockStmt(Block stmt);
//     R visitClassStmt(Class stmt);
//     R visitExpressionStmt(Expression stmt);
//     R visitFunctionStmt(Function stmt);
//     R visitIfStmt(If stmt);
//     R visitPrintStmt(Print stmt);
//     R visitReturnStmt(Return stmt);
//     R visitVarStmt(Var stmt);
//     R visitWhileStmt(While stmt);
//   }
//
//   // Nested Stmt classes here...
//
//   abstract <R> R accept(Visitor<R> visitor);
// }

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
