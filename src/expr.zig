const std = @import("std");
const assert = std.debug.assert;

const Interpreter = @import("interpreter.zig");
const Token = @import("token.zig");
const formatNumber = @import("root.zig").formatNumber;
const logger = @import("logger.zig");

pub const Expr = union(enum) {
    assign: Assign,
    binary: Binary,
    call: Call,
    grouping: *Expr,
    literal: Value,
    logical: Logical,
    unary: Unary,
    variable: Token,

    pub const Assign = struct {
        name: Token,
        value: *Expr,
    };

    pub const Binary = struct {
        left: *Expr,
        operator: Token,
        right: *Expr,
    };

    pub const Call = struct {
        callee: *Expr,
        paren: Token,
        arguments: []*Expr,
    };

    pub const Logical = struct {
        left: *Expr,
        operator: Token,
        right: *Expr,
    };

    pub const Unary = struct {
        operator: Token,
        right: *Expr,
    };

    pub const LoxReturnValue = union(enum) {
        nil: void,
        ret: Value,

        pub fn fromValue(value: ?Value) LoxReturnValue {
            return if (value) |v| switch (v) {
                .nil => .{ .ret = Value.Nil },
                else => |x| .{ .ret = x },
            } else .{ .ret = Value.Nil };
        }

        pub fn toValue(self: *LoxReturnValue) Value {
            return switch (self.*) {
                .nil => Value.Nil,
                .ret => |x| x,
            };
        }
    };

    pub const Value = union(enum) {
        bool: bool,
        callable: *LoxCallable,
        function: *LoxFunction,
        nil: void,
        num: f64,
        ret: *LoxReturnValue,
        str: []const u8,

        pub const Nil = Value{ .nil = {} };
        pub const True = Value{ .bool = true };
        pub const False = Value{ .bool = !true };

        pub const LoxCallable = struct { // native function
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

        pub const LoxFunction = struct {
            /// `Context` pointer to hold function-specific data.
            context: *anyopaque,

            arityFn: *const fn (*anyopaque) usize,
            callFn: *const fn (*anyopaque, *Interpreter, []Value) Value,
            toStringFn: *const fn (*anyopaque) []const u8,

            pub fn arity(self: *LoxFunction) usize {
                return self.arityFn(self.context);
            }

            pub fn call(self: *LoxFunction, interpreter: *Interpreter, arguments: []Value) Value {
                return self.callFn(self.context, interpreter, arguments);
            }

            pub fn toString(self: *LoxFunction) []const u8 {
                return self.toStringFn(self.context);
            }
        };

        pub inline fn from(x: anytype) Value {
            return switch (@TypeOf(x)) {
                usize, i32, comptime_int => Value{ .num = @as(f64, @floatFromInt(x)) },
                f64, comptime_float => Value{ .num = x },
                void => Nil,
                else => |@"type"| {
                    logger.err(.{}, @src(), "Unimplemented case for type {any}.", .{@"type"});
                    @panic("Unimplemented");
                },
            };
        }

        pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .bool => |val| try std.fmt.format(writer, "{}", .{val}),
                .callable => |val| try std.fmt.format(writer, "{any}", .{val}),
                .function => |val| try std.fmt.format(writer, "{any}", .{val}),
                .nil => try std.fmt.format(writer, "nil", .{}),
                .num => |val| try formatNumber(writer, val),
                .ret => |val| try std.fmt.format(writer, "{any}", .{val}),
                .str => |val| try std.fmt.format(writer, "{s}", .{val}),
            }
        }
    };
};

// See https://craftinginterpreters.com/appendix-ii.html#expressions
//
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

// assign →
// binary → expression operator expression ;
// call → primary ( "(" arguments? ")" )* ;
// grouping → "(" expression ")" ;
// literal → NUMBER | STRING | "true" | "false" | "nil" ;
// logic_or       → logic_and ( "or" logic_and )* ;
// logic_and      → equality ( "and" equality )* ;
// unary → ( "-" | "!" ) expression ;
// variable →
//
// operator → "==" | "!=" | "<" | "<=" | ">" | ">=" | "+"  | "-"  | "*" | "/" ;
//
// "Binary    : Expr left, Token operator, Expr right"
// "Call     : Expr callee, Token paren, List<Expr> arguments"
// "Logical  : Expr left, Token operator, Expr right",
//      Avoids binary visit method see if operator is one of the logical
//      operators and use a different code path to handle the short circuiting.
// "Unary    : Token operator, Expr right"
// "Literal  : Object value"
//
// arguments: []*Expr, // → expression ( "," expression )* ;

// Docs

// Value.LoxReturnValue
// If we have a return value, we evaluate it, otherwise, we use nil.
// Then we take that value and wrap it in a custom exception class and
// throw it. We want this to unwind all the way to where the function
// call began, the call() method in LoxFunction.

// Value.from()
// See https://gitlab.com/andreyorst/lox/-/blob/main/src/zig/lox/value.zig?ref_type=heads#L253
