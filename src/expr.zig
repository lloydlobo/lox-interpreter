const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;

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

            pub fn call(
                self: *const LoxCallable,
                interpreter: *Interpreter,
                arguments: []Value,
            ) Value {
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
                .bool => |bool_| try std.fmt.format(writer, "{}", .{bool_}),
                .callable => |lox_callable_ptr| try std.fmt.format(writer, "{s}(<arity {d}>)", .{
                    lox_callable_ptr.*.toString(),
                    lox_callable_ptr.*.arity(),
                }),
                .function => |lox_function_ptr| try std.fmt.format(writer, "{s}(<arity {d}>)", .{
                    lox_function_ptr.*.toString(),
                    lox_function_ptr.*.arity(),
                }),
                .nil => try std.fmt.format(writer, "nil", .{}),
                .num => |@"f64"| try formatNumber(writer, @"f64"),
                .ret => |lox_return_value_ptr| try std.fmt.format(writer, "{any}", .{lox_return_value_ptr}),
                .str => |@"[]const u8"| try std.fmt.format(writer, "{s}", .{@"[]const u8"}),
            }
        }
    };

    /// Converts union value to a string literal representing the name.
    pub fn toString(self: Expr) []const u8 {
        return @tagName(self);
    }
};

// Property: Reversing a list twice should return the original list
fn reverseProperty(list: []i32) bool {
    const reversed1 = reverse(list);
    const reversed2 = reverse(reversed1);
    return std.mem.eql(i32, list, reversed2);
}

// Function to reverse a list
fn reverse(list: []i32) []i32 {
    const result = list;
    std.mem.reverse(i32, result);
    return result;
}

// Generate random list of integers
fn generateRandomList(allocator: *std.mem.Allocator, size: usize) ![]i32 {
    const list = try allocator.alloc(i32, size);
    var prng: std.rand.Xoshiro256 = std.rand.DefaultPrng.init(0);
    var random: std.rand.Random = prng.random();
    for (list) |*item| {
        item.* = random.int(i32);
    }
    return list;
}

// Property-based test
test "reverse property" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator: std.mem.Allocator = &arena.allocator;

    const num_tests = 100;
    var i: usize = 0;
    while (i < num_tests) : (i += 1) {
        const size = std.rand.DefaultPrng
            .init(i)
            .random()
            .intRangeAtMost(usize, 0, 100);
        const list: []i32 = try generateRandomList(allocator, size);

        try testing.expect(reverseProperty(list));
    }
}

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
