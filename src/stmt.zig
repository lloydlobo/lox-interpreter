const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;
const fmt = std.fmt;
const FormatOptions = fmt.FormatOptions;

const Expr = @import("expr.zig").Expr;
const Token = @import("token.zig");
const debug = @import("debug.zig");
const logger = @import("logger.zig");

/// Grammar:
/// `statement → block | break_stmt | expr_stmt | for_stmt | if_stmt | print_stmt | var_stmt | while_stmt  ;`
pub const Stmt = union(enum) {
    block: Block,
    break_stmt: Break,
    class: Class,
    expr_stmt: Expression,
    function: Function,
    if_stmt: If,
    print_stmt: Print,
    return_stmt: Return,
    var_stmt: Var,
    while_stmt: While,

    comptime {
        assert(@sizeOf(@This()) == 96);
        assert(@alignOf(@This()) == 8);
    }

    /// Converts union value to a string literal representing the name.
    pub fn toString(self: Stmt) []const u8 {
        return @tagName(self);
    }

    pub const Block = []Stmt;

    pub const Break = ?*Expr;

    pub const Class = struct {
        name: Token,
        methods: []Stmt.Function,

        comptime {
            assert(@sizeOf(@This()) == 72);
            assert(@alignOf(@This()) == 8);
        }

        pub const class_kind = "class";

        pub fn create(allocator: Allocator) !*Class {
            const self = try allocator.create(Class);
            errdefer allocator.destroy(self);
            if (comptime debug.is_trace_garbage_collector) {
                logger.debug(.default, @src(), "{} allocate {} for {s}", .{ @intFromPtr(&self), @sizeOf(Class), @typeName(Class) });
            }

            return self;
        }
    };

    pub const Expression = *Expr;

    pub const Function = struct { // should implement Callable
        body: []Stmt, // allocate separately?
        name: Token,
        parameters: []Token,

        comptime {
            assert(@sizeOf(@This()) == 88);
            assert(@alignOf(@This()) == 8);
        }

        pub const function_kind = "function";
        pub const method_kind = "method";

        pub fn create(allocator: Allocator) !*Function {
            const self = try allocator.create(Function);
            errdefer allocator.destroy(self);
            if (comptime debug.is_trace_garbage_collector) {
                logger.debug(.default, @src(), "{} allocate {} for {s}", .{
                    @intFromPtr(&self),  @sizeOf(Function),
                    @typeName(Function),
                });
            }

            return self;
        }
    };

    pub const If = struct {
        condition: *Expr,
        /// `else` is bound to the nearest `if` that precedes it.
        else_branch: ?*Stmt,
        then_branch: *Stmt,

        comptime {
            assert(@sizeOf(@This()) == 24);
            assert(@alignOf(@This()) == 8);
        }
    };

    pub const Print = *Expr;

    pub const Return = struct {
        keyword: Token,
        value: ?*Expr,

        comptime {
            assert(@sizeOf(@This()) == 64);
            assert(@alignOf(@This()) == 8);
        }
    };

    pub const Var = struct {
        initializer: ?*Expr,
        name: Token,

        comptime {
            assert(@sizeOf(@This()) == 64);
            assert(@alignOf(@This()) == 8);
        }
    };

    pub const While = struct {
        body: *Stmt,
        condition: *Expr,

        comptime {
            assert(@sizeOf(@This()) == 16);
            assert(@alignOf(@This()) == 8);
        }
    };

    pub fn extractThisExpr(self: *const Stmt) ?Expr.This {
        switch (self.*) {
            .block => |block| {
                for (block) |*innerStmt| {
                    if (extractThisExpr(innerStmt)) |result| {
                        return result;
                    }
                }
                return null;
            },
            .break_stmt => return null,
            .class => |class| {
                for (class.methods) |method| {
                    if (extractThisExpr(&Stmt{ .function = method })) |result| {
                        return result;
                    }
                }
                return null;
            },
            .expr_stmt => |expr_stmt| return expr_stmt.findThisInExpr(),
            .function => |function| {
                for (function.body) |*innerStmt| {
                    if (extractThisExpr(innerStmt)) |result| {
                        return result;
                    }
                }
                return null;
            },
            .if_stmt => |if_stmt| {
                if (if_stmt.condition.findThisInExpr()) |result| return result;
                if (extractThisExpr(if_stmt.then_branch)) |result| return result;
                if (if_stmt.else_branch) |elseBranch| {
                    if (extractThisExpr(elseBranch)) |result| return result;
                }
                return null;
            },
            .print_stmt => |print_stmt| return print_stmt.findThisInExpr(),
            .return_stmt => |return_stmt| return return_stmt.value.?.findThisInExpr(),
            .var_stmt => |var_stmt| return var_stmt.initializer.?.findThisInExpr(),
            .while_stmt => |while_stmt| return while_stmt.condition.findThisInExpr() orelse
                extractThisExpr(while_stmt.body),
        }
    }
};

test "basic usage" {
    try testing.expectEqual(0, @sizeOf(@This()));
    try testing.expectEqual(1, @alignOf(@This()));

    // std.log.warn("{}", .{@sizeOf(Stmt.Class)});
}

// See https://craftinginterpreters.com/appendix-ii.html#statements
//
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
//
// See https://craftinginterpreters.com/control-flow.html
//
// # Control Flow
//
// The process of proving that the answer to the first two questions is “no”,
// Alan Turing and Alonzo Church devised a precise answer to the last
// question—a definition of what kinds of functions are computable. They each
// crafted a tiny system with a minimum set of machinery that is still powerful
// enough to compute any of a (very) large class of functions.
//
// They proved the answer to the first question is “no” by showing that the
// function that returns the truth value of a given statement is not a
// computable one.
//
// These are now considered the “computable functions”. Turing’s system is
// called a Turing machine. Church’s is the lambda calculus. Both are still
// widely used as the basis for models of computation and, in fact, many modern
// functional programming languages use the lambda calculus at their core.
//
// We can divide control flow roughly into two kinds:
//
// 1. Conditional or branching control flow is used to not execute some piece
// of code. Imperatively, think of it as jumping ahead over a region of code.
//
// 2. Looping control flow executes a chunk of code more than once. It jumps
// back so that you can do something again. Since you don’t usually want
// infinite loops, it typically has some conditional logic to know when to stop
// looping as well.
//
// The conditional operator is also called the “ternary” operator because it’s
// the only operator in C that takes three operands.
