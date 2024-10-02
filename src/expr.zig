const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;

const Callable = @import("callable.zig");
const Interpreter = @import("interpreter.zig");
const Token = @import("token.zig");
const Value = @import("value.zig").Value;
const formatNumber = @import("root.zig").formatNumber;
const logger = @import("logger.zig");
const root = @import("root.zig");

pub const Expr = union(enum) {
    assign: Assign,
    binary: Binary,
    call: Call,
    get: Get,
    grouping: *Expr,
    literal: Value,
    logical: Logical,
    set: Set,
    unary: Unary,
    variable: Token,

    comptime {
        assert(@sizeOf(@This()) == 88);
        assert(@alignOf(@This()) == 8);
    }

    /// Converts union value to a string literal representing the name.
    pub fn toString(self: Expr) []const u8 {
        return @tagName(self);
    }

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

    pub const Get = struct {
        object: *Expr,
        name: Token,
    };

    pub const Logical = struct {
        left: *Expr,
        operator: Token,
        right: *Expr,
    };

    pub const Set = struct {
        object: *Expr,
        name: Token,
        value: *Expr,
    };

    pub const Unary = struct {
        operator: Token,
        right: *Expr,
    };
};

test "Expr ─ basic usage" {
    try testing.expectEqual(0, @sizeOf(@This()));
    try testing.expectEqual(1, @alignOf(@This()));
}

// // Property: Reversing a list twice should return the original list
// fn reverseProperty(list: []i32) bool {
//     const reversed1 = reverse(list);
//     const reversed2 = reverse(reversed1);
//     return std.mem.eql(i32, list, reversed2);
// }
//
// // Function to reverse a list
// fn reverse(list: []i32) []i32 {
//     const result = list;
//     std.mem.reverse(i32, result);
//     return result;
// }
//
// // Generate random list of integers
// fn generateRandomList(allocator: *std.mem.Allocator, size: usize) ![]i32 {
//     const list = try allocator.alloc(i32, size);
//     var prng: std.rand.Xoshiro256 = std.rand.DefaultPrng.init(0);
//     var random: std.rand.Random = prng.random();
//     for (list) |*item| {
//         item.* = random.int(i32);
//     }
//     return list;
// }
//
// // Property-based test
// test "reverse property" {
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//     const allocator: std.mem.Allocator = &arena.allocator;
//
//     const num_tests = 100;
//     var i: usize = 0;
//     while (i < num_tests) : (i += 1) {
//         const size = std.rand.DefaultPrng
//             .init(i)
//             .random()
//             .intRangeAtMost(usize, 0, 100);
//         const list: []i32 = try generateRandomList(allocator, size);
//
//         try testing.expect(reverseProperty(list));
//     }
// }

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

//
// assignment     → ( call "." )? IDENTIFIER "=" assignment | logic_or ;
// binary         → expression operator expression ;
// call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
// grouping       → "(" expression ")" ;
// literal        → NUMBER | STRING | "true" | "false" | "nil" ;
// logic_and      → equality ( "and" equality )* ;
// logic_or       → logic_and ( "or" logic_and )* ;
// unary          → ( "-" | "!" ) expression ;
// variable       →
//
// operator       → "==" | "!=" | "<" | "<=" | ">" | ">=" | "+"  | "-"  | "*" | "/" ;
//
// "Binary        : Expr left, Token operator, Expr right",
// "Call          : Expr callee, Token paren, List<Expr> arguments",
// "Get           : Expr object, Token name",
// "Literal       : Object value",
// "Logical       : Expr left, Token operator, Expr right",,
// "Set           : Expr object, Token name, Expr value",
// "Unary         : Token operator, Expr right",

// "Block         : List<Stmt> statements",
// "Class         : Token name, List<Stmt.Function> methods",
// "Expression    : Expr expression",
// "Function      : Token name, List<Token> params," + " List<Stmt> body",
// "If            : Expr condition, Stmt thenBranch," +

// arguments      → expression ( "," expression )* ;
//
// declaration    → classDecl | funDecl | varDecl | statement ;
//
// funDecl        → "fun" function ;
// function       → IDENTIFIER "(" parameters? ")" block ;
// parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
//
// classDecl      → "class" IDENTIFIER "{" function* "}" ;

// Note: The new classDecl rule relies on the function rule we defined earlier. To refresh your memory:

//
//
// Docs
//
//

// Value.LoxReturnValue
// If we have a return value, we evaluate it, otherwise, we use nil.
// Then we take that value and wrap it in a custom exception class and
// throw it. We want this to unwind all the way to where the function
// call began, the call() method in LoxFunction.

// Value.from()
// See https://gitlab.com/andreyorst/lox/-/blob/main/src/zig/lox/value.zig?ref_type=heads#L253

// Expr.Set
// Unlike getters, setters don’t chain. However, the reference to call allows
// any high-precedence expression before the last dot, including any number of
// getters, as in:
