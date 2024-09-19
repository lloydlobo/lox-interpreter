const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const fmt = std.fmt;
const FormatOptions = fmt.FormatOptions;

const Expr = @import("expr.zig").Expr;
const Token = @import("token.zig").Token;

/// statement        → expr
///                  | break_stmt
///                  | for_stmt
///                  | if_stmt
///                  | print
///                  | var_stmt
///                  | while_stmt
///                  | block ;
pub const Stmt = union(enum) {
    break_stmt: ?*Expr,
    expr: *Expr,
    if_stmt: If, // → "if" "(" expression ")" statement ( "else" statement )? ;
    print: *Expr,
    var_stmt: Var,
    while_stmt: While,
    block: []Stmt,

    /// "If         : Expr condition, Stmt thenBranch," + " Stmt elseBranch",
    ///
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
