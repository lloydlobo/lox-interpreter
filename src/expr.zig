const std = @import("std");

const Token = @import("token.zig").Token;
const formatNumber = @import("root.zig").formatNumber;

pub const Expr = union(enum) {
    /// ?
    assign: Assign,
    /// → expression operator expression ;
    binary: Binary,
    /// → "(" expression ")" ;
    grouping: *Expr,
    /// → NUMBER | STRING | "true" | "false" | "nil" ;
    literal: Value,
    /// logic_or       → logic_and ( "or" logic_and )* ;
    /// logic_and      → equality ( "and" equality )* ;
    logical: Logical,
    /// → ( "-" | "!" ) expression ;
    unary: Unary,
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

    // We could reuse the existing Expr.Binary class for these two new expressions since they have the same fields. But then visitBinaryExpr() would have to check to see if the operator is one of the logical operators and use a different code path to handle the short circuiting. I think it’s cleaner to define a new class for these operators so that they get their own visit method.
    pub const Logical = struct {
        left: *Expr,
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

// See https://craftinginterpreters.com/appendix-ii.html#expressions
// A2.1 Expressions
//
// Expressions are the first syntax tree nodes we see, introduced in
// “Representing Code”. The main Expr class defines the visitor interface used
// to dispatch against the specific expression types, and contains the other
// expression subclasses as nested classes.
//
// package com.craftinginterpreters.lox;
//
// import java.util.List;
//
// abstract class Expr {
//   interface Visitor<R> {
//     R visitAssignExpr(Assign expr);
//     R visitBinaryExpr(Binary expr);
//     R visitCallExpr(Call expr);
//     R visitGetExpr(Get expr);
//     R visitGroupingExpr(Grouping expr);
//     R visitLiteralExpr(Literal expr);
//     R visitLogicalExpr(Logical expr);
//     R visitSetExpr(Set expr);
//     R visitSuperExpr(Super expr);
//     R visitThisExpr(This expr);
//     R visitUnaryExpr(Unary expr);
//     R visitVariableExpr(Variable expr);
//   }
//
//   // Nested Expr classes here...
//
//   abstract <R> R accept(Visitor<R> visitor);
// }
