const std = @import("std");

const Obj = @import("object.zig").Obj;
const Interpreter = @import("interpreter.zig").Interpreter;
const Token = @import("token.zig").Token;
const formatNumber = @import("root.zig").formatNumber;

const NAN_BOXING = false; // see https://gitlab.com/andreyorst/lox/-/blob/main/src/zig/lox/value.zig?ref_type=heads#L21

pub const Expr = union(enum) {
    /// ?
    assign: Assign,
    /// → expression operator expression ;
    binary: Binary,
    /// → primary ( "(" arguments? ")" )* ;
    call: Call,
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

    /// "Binary    : Expr left, Token operator, Expr right"
    pub const Binary = struct {
        left: *Expr,
        /// → "==" | "!=" | "<" | "<=" | ">" | ">=" | "+"  | "-"  | "*" | "/" ;
        operator: Token,
        right: *Expr,
    };

    /// "Call     : Expr callee, Token paren, List<Expr> arguments"
    pub const Call = struct {
        callee: *Expr,
        paren: Token,
        /// → expression ( "," expression )* ;
        arguments: []*Expr, // List<Expr>
    };

    /// "Logical  : Expr left, Token operator, Expr right",
    // We could reuse the existing Expr.Binary class for these two new expressions since they have the same fields. But then visitBinaryExpr() would have to check to see if the operator is one of the logical operators and use a different code path to handle the short circuiting. I think it’s cleaner to define a new class for these operators so that they get their own visit method.
    pub const Logical = struct {
        left: *Expr,
        /// → "==" | "!=" | "<" | "<=" | ">" | ">=" | "+"  | "-"  | "*" | "/" ;
        operator: Token,
        right: *Expr,
    };

    /// "Unary    : Token operator, Expr right"
    pub const Unary = struct {
        /// → "==" | "!=" | "<" | "<=" | ">" | ">=" | "+"  | "-"  | "*" | "/" ;
        operator: Token,
        right: *Expr,
    };

    /// "Literal  : Object value"
    pub const Value = union(enum) { // UnionValue
        bool: bool,
        callable: *LoxCallable,
        nil: void,
        num: f64,
        obj: *Obj,
        str: []const u8,

        // pub const Nil = Value{ .nil = undefined };
        pub const Nil = Value{ .nil = {} };
        pub const True = Value{ .bool = true };
        pub const False = Value{ .bool = !true };

        pub const LoxCallable = struct {
            arityFn: *const fn () usize,
            callFn: *const fn (*Interpreter, []Value) Value,
            toStringFn: *const fn () []const u8,

            pub fn arity(self: *const LoxCallable) usize {
                return self.arityFn();
            }

            pub fn call(self: *const LoxCallable, interpreter: *Interpreter, arguments: []Value) Value {
                return self.callFn(interpreter, arguments);
            }

            pub fn toString(self: *const LoxCallable) []const u8 {
                return self.toStringFn();
            }
        };
        // pub fn LoxCallable(comptime T: type) type {
        //     return struct {
        //         arity: fn () u32,
        //         call: fn (self: *T, arguments: []const T) anyerror!T,
        //         toString: fn () []const u8,
        //     };
        // }

        // See https://github.com/raulgrell/zox/blob/master/src/value.zig#L46
        // See https://github.com/raulgrell/zox/blob/master/src/value.zig#L250
        pub fn isObj(self: Value) bool {
            return self == .obj;
        }

        // (defrecord LoxCallable [arity, function]
        //   IStringable
        //   (tostring [_] "#<native fn>"))

        // See https://gitlab.com/andreyorst/lox/-/blob/main/src/zig/lox/value.zig?ref_type=heads#L253
        pub inline fn from(x: anytype) Value {
            return switch (@TypeOf(x)) {
                usize, i32, comptime_int => Value{ .num = @as(f64, @floatFromInt(x)) },
                f64, comptime_float => Value{ .num = x },
                void => Nil,
                *Obj => Value{ .obj = x },
                else => unreachable,
            };
        }

        pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .bool => |val| try std.fmt.format(writer, "{}", .{val}),
                .nil => try std.fmt.format(writer, "nil", .{}),
                .num => |val| try formatNumber(writer, val),
                // .obj => |obj| try std.fmt.format(writer, "{}", .{obj}),
                .obj => |obj| try obj.print(writer),
                .callable => |val| try std.fmt.format(writer, "{any}", .{val}),
                .str => |val| try std.fmt.format(writer, "{s}", .{val}),
            }
        }
    };
};

pub const NanBoxedValue = packed struct {
    data: u64,
};

// See https://gitlab.com/andreyorst/lox/-/blob/main/src/zig/lox/value.zig?ref_type=heads#L345
// pub const Value = if (NAN_BOXING) NanBoxedValue else UnionValue;
pub const EitherValue = if (NAN_BOXING) Expr.NanBoxedValue else Expr.Value;

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
