//! A recursive descent parser. The main entry point is the `parse` function.

const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;

const AstPrinter = @import("astprinter.zig").AstPrinter;
const Expr = @import("expr.zig").Expr;
const Scanner = @import("scanner.zig").Scanner;
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const TypeSets = @import("token.zig").TypeSets;
const Value = @import("value.zig").Value;
const debug = @import("debug.zig");
const root = @import("root.zig");
const tokenError = @import("main.zig").tokenError;

const Parser = @This();

allocator: Allocator,
current: usize = 0, // index
tokens: std.MultiArrayList(Token),
tokens_len: usize,

comptime {
    assert(@sizeOf(@This()) == 56);
    assert(@alignOf(@This()) == 8);
}

const Error = error{ parse_error, io_error } || Allocator.Error;

pub fn init(tokens: []const Token, allocator: Allocator) Allocator.Error!Parser {
    var list = std.MultiArrayList(Token){};
    try list.ensureTotalCapacity(allocator, tokens.len);
    for (tokens) |token| {
        list.appendAssumeCapacity(token);
    }

    return .{
        .allocator = allocator,
        .current = 0,
        .tokens = list,
        .tokens_len = list.len,
    };
}

// PERF: Use tagged union comptime type T, for better allocation precision.
fn createExpr(self: *Parser, value: Expr) Allocator.Error!*Expr {
    const expr = try self.allocator.create(Expr);
    errdefer self.allocator.destroy(expr);
    expr.* = value;

    return expr;
}

// PERF: Use tagged union comptime type T, for better allocation precision.
fn createStmt(self: *Parser, value: Stmt) Allocator.Error!*Stmt {
    const stmt = try self.allocator.create(Stmt);
    errdefer self.allocator.destroy(stmt);
    stmt.* = value;

    return stmt;
}

fn parseError(token: Token, comptime message: []const u8) Error {
    tokenError(token, message);

    return Error.parse_error;
}

fn currentType(self: *const Parser) Token.Type {
    assert(self.current < self.tokens_len);

    // see user: xy1 src/main.zig -> fn current
    return self.tokens.items(.type)[self.current];
}

fn isAtEnd(self: *const Parser) bool {
    return (self.currentType() == .eof);
}

fn check(self: *const Parser, @"type": Token.Type) bool {
    return switch (self.currentType()) {
        .eof => false, // if is at end
        else => |x| (x == @"type"),
    };
}

fn checkPrevious(self: *const Parser, @"type": Token.Type) bool {
    return switch (self.previousType()) {
        .eof => false, // if is at end
        else => |x| (x == @"type"),
    };
}

fn peek(self: *const Parser) Token {
    assert(self.current >= 0); // undefined implementaion logic if called when current index is 0

    return self.tokens.get(self.current);
}

fn previous(self: *const Parser) Token {
    assert(self.current > 0);

    return self.tokens.get(self.current - 1);
}

fn previousLiteral(self: *const Parser) ?Token.Literal {
    assert(self.current > 0);

    return self.tokens.items(.literal)[self.current - 1];
}

fn previousType(self: *const Parser) Token.Type {
    assert(self.current > 0);

    return self.tokens.items(.type)[self.current - 1];
}

fn previousValue(self: *const Parser) Value {
    return if (self.previousLiteral()) |literal| switch (literal) {
        .str => |val| .{ .str = val },
        .num => |val| .{ .num = val },
    } else unreachable;
}

/// match is short for consumeToken()
fn match(self: *Parser, comptime types: *const Token.TypeSet) bool {
    // fn match(self: *Parser, types: anytype) bool {
    //    return inline for (std.meta.fields(@TypeOf(types))) |field| {
    //        if (self.check(@field(types, field.name))) {
    //            _ = self.advance();
    //            break true;
    //        }
    //    } else false;
    // }
    return (types.contains(self.currentType()) and blk: {
        _ = self.advance();
        break :blk true;
    });
}

fn consume(self: *Parser, @"type": Token.Type, comptime message: []const u8) Error!Token {
    if (self.check(@"type")) {
        return self.advance();
    } else {
        return parseError(self.peek(), message);
    }
}

fn advance(self: *Parser) Token {
    if (!self.isAtEnd()) {
        self.current += 1;
    }
    assert((self.current != 0) and (self.current < self.tokens_len));

    return self.previous();
}

//
//
// Grammar implementation
//
// Precedence: expression → equality → comparison → term → factor → unary → primary
//
//

/// primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
fn primary(self: *Parser) Error!*Expr {
    if (self.match(TypeSets.false)) {
        return try self.createExpr(.{ .literal = Value.False });
    }
    if (self.match(TypeSets.true)) {
        return try self.createExpr(.{ .literal = Value.True });
    }
    if (self.match(TypeSets.nil)) {
        return try self.createExpr(.{ .literal = Value.Nil });
    }
    if (self.match(TypeSets.number_string)) {
        return try self.createExpr(.{ .literal = self.previousValue() });
    }
    if (self.match(TypeSets.left_paren)) { //`(` grouping precedence - recursion: primary() |> expression()
        const expr = try self.expression();
        _ = try self.consume(.right_paren, "Expect ')' after expression."); //`)`

        return try self.createExpr(.{ .grouping = expr });
    }
    if (self.match(TypeSets.identifier)) { // variable precedence: from declaration() |> primary()
        return try self.createExpr(.{ .variable = self.previous() });
    }

    return parseError(self.peek(), "Expect expression.");
}

fn call(self: *Parser) Error!*Expr {
    var expr: *Expr = try self.primary();

    // The outer while loop there corresponds to the * in the grammar rule. We
    // zip along the tokens building up a chain of calls and gets as we find
    // parentheses and dots, like so:
    // See https://craftinginterpreters.com/image/classes/zip.png
    while (true) {
        if (self.match(TypeSets.left_paren)) {
            expr = try self.finishCall(expr);
        } else if (self.match(TypeSets.dot)) {
            const name: Token = try self.consume(
                .identifier,
                "Expect property name after '.'.",
            );
            expr = try self.createExpr(.{ .get = .{
                .name = name,
                .value = expr,
            } });

        } else {
            break;
        }
    }

    return expr;
}

fn finishCall(self: *Parser, callee: *Expr) Error!*Expr {
    var arguments = std.ArrayList(*Expr).init(self.allocator);
    errdefer arguments.deinit();

    var arg_count: u8 = 0;
    if (!self.check(.right_paren)) {
        var do = true;
        while (do or self.match(TypeSets.comma)) {
            do = false;
            arg_count += 1; // defer?
            try arguments.append(try self.expression());

            if (arg_count == 255) {
                return parseError(self.peek(), "Cannot have more than 255 arguments.");
            }
        }
    }
    const paren: Token = try self.consume(.right_paren, "Expect ')' after arguments.");

    const expr: *Expr = try self.createExpr(.{ .call = .{
        .callee = callee,
        .paren = paren,
        .arguments = try arguments.toOwnedSlice(),
    } });

    return expr;
}

/// unary → ( "!" | "-" ) unary | primary ;
fn unary(self: *Parser) Error!*Expr {
    if (self.match(TypeSets.bang_minus)) {
        const operator: Token = self.previous();
        const right: *Expr = try self.unary(); // recursion to same precedence level

        return try self.createExpr(.{ .unary = .{
            .operator = operator,
            .right = right,
        } });
    }

    return try self.call(); // ...previously jump to `primary()`
}

/// factor → unary ( ( "/" | "*" ) unary )* ;
fn factor(self: *Parser) Error!*Expr {
    var expr: *Expr = try self.unary();

    while (self.match(TypeSets.slash_star)) {
        const operator: Token = self.previous();
        const right: *Expr = try self.unary();

        expr = try self.createExpr(.{ .binary = .{
            .left = expr,
            .operator = operator,
            .right = right,
        } });
    }

    return expr;
}

/// term → factor ( ( "-" | "+" ) factor )* ;
fn term(self: *Parser) Error!*Expr {
    var expr: *Expr = try self.factor();

    while (self.match(TypeSets.minus_plus)) {
        const operator: Token = self.previous();
        const right: *Expr = try self.factor();

        expr = try self.createExpr(.{ .binary = .{
            .left = expr,
            .operator = operator,
            .right = right,
        } });
    }

    return expr;
}

/// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
fn comparison(self: *Parser) Error!*Expr {
    var expr: *Expr = try self.term();

    while (self.match(TypeSets.comparison)) {
        const operator: Token = self.previous();
        const right: *Expr = try self.term();

        expr = try self.createExpr(.{ .binary = .{
            .left = expr,
            .operator = operator,
            .right = right,
        } });
    }

    return expr;
}

/// equality → comparison ( ( "!=" | "==" ) comparison )* ;
fn equality(self: *Parser) Error!*Expr {
    var expr: *Expr = try self.comparison();

    while (self.match(TypeSets.equality)) {
        const operator: Token = self.previous();
        const right: *Expr = try self.comparison();

        expr = try self.createExpr(.{ .binary = .{
            .left = expr,
            .operator = operator,
            .right = right,
        } });
    }

    return expr;
}

fn andExpr(self: *Parser) Error!*Expr {
    var expr: *Expr = try self.equality();

    while (self.match(TypeSets.@"and")) {
        const operator: Token = self.previous();
        const right: *Expr = try self.equality();

        expr = try self.createExpr(.{ .logical = .{
            .left = expr,
            .operator = operator,
            .right = right,
        } });
    }

    return expr;
}

fn orExpr(self: *Parser) Error!*Expr {
    var expr: *Expr = try self.andExpr();

    while (self.match(TypeSets.@"or")) {
        const operator: Token = self.previous();
        const right: *Expr = try self.andExpr();

        expr = try self.createExpr(.{ .logical = .{
            .left = expr,
            .operator = operator,
            .right = right,
        } });
    }

    return expr;
}

fn assignment(self: *Parser) Error!*Expr {
    const expr: *Expr = try self.orExpr();

    if (self.match(TypeSets.assignment)) {
        const equals: Token = self.previous();
        const value: *Expr = try self.assignment();

        return switch (expr.*) {
            .variable => |name| try self.createExpr(.{ .assign = .{
                .name = name,
                .value = value,
            } }),
            else => parseError(equals, "Invalid assignment target."),
        };
    }

    return expr;
}

/// expression → equality ;
fn expression(self: *Parser) Error!*Expr {
    return try self.assignment();
}

fn synchronize(self: *Parser) void {
    _ = self.advance();

    while (!self.isAtEnd()) {
        if (self.checkPrevious(.semicolon)) {
            return;
        }
        switch (self.currentType()) {
            .class, .fun, .@"var", .@"for", .@"if", .@"while", .print, .@"return" => return,
            else => {},
        }

        _ = self.advance();
    }
}

fn printStatement(self: *Parser) Error!Stmt {
    const value: *Expr = try self.expression();
    _ = try self.consume(.semicolon, "Expect ';' after value.");

    return .{ .print_stmt = value };
}

fn expressionStatement(self: *Parser) Error!Stmt {
    const value: *Expr = try self.expression(); //> (*Expr) union(enum)
    _ = try self.consume(.semicolon, "Expect ';' after expression.");

    return .{ .expr_stmt = value };
}

fn block(self: *Parser) Error![]Stmt {
    assert(self.checkPrevious(.left_brace));

    var statements = std.ArrayList(Stmt).init(self.allocator);
    errdefer statements.deinit();

    while (!self.check(.right_brace) and !self.isAtEnd()) {
        if ((try self.declaration())) |decl| {
            try statements.append(decl);
        }
    }
    _ = try self.consume(.right_brace, "Expect '}}' after block.");

    return try statements.toOwnedSlice();
}

fn ifStatement(self: *Parser) Error!Stmt {
    assert(self.checkPrevious(.@"if"));

    _ = try self.consume(.left_paren, "Expect '(' after 'if'.");
    const condition = try self.expression();
    _ = try self.consume(.right_paren, "Expect ')' after if condition.");

    const stmt: Stmt = .{ .if_stmt = .{
        .condition = condition,
        .then_branch = try self.createStmt(try self.statement()),
        .else_branch = blk: {
            if (self.match(TypeSets.@"else")) {
                break :blk try self.createStmt(try self.statement());
            } else {
                break :blk null;
            }
        },
    } };

    return stmt;
}

fn whileStatement(self: *Parser) Error!Stmt {
    assert(self.checkPrevious(.@"while"));

    _ = try self.consume(.left_paren, "Expect '(' after 'while'.");
    const condition = try self.expression();
    _ = try self.consume(.right_paren, "Expect ')' after condition.");

    const stmt: Stmt = .{ .while_stmt = .{
        .condition = condition,
        .body = try self.createStmt(try self.statement()),
    } };

    return stmt;
}

/// for_stmt → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
fn forStatement(self: *Parser) Error!Stmt {
    assert(self.checkPrevious(.@"for"));

    _ = try self.consume(.left_paren, "Expect '(' after 'for'.");

    // Clause 1
    const initializer = blk: {
        if (self.match(TypeSets.semicolon)) {
            break :blk null; // no initializer
        } else if (self.match(TypeSets.@"var")) {
            break :blk try self.varDeclaration(); // variable
        } else {
            break :blk try self.expressionStatement();
        }
    };

    // Clause 2
    var condition: ?*Expr = blk: {
        if (!self.check(.semicolon)) {
            break :blk try self.expression();
        } else {
            break :blk null;
        }
    };
    _ = try self.consume(.semicolon, "Expect ';' after loop condition.");

    // Clause 3
    const increment = blk: {
        if (!self.check(.right_paren)) {
            break :blk try self.expression();
        } else {
            break :blk null;
        }
    };
    _ = try self.consume(.right_paren, "Expect ')' after for clauses.");

    var body: *Stmt = try self.createStmt(try self.statement());

    // Simplifying the structure by processing in reverse order:
    // 3 > 2 > 1

    if (increment) |expr| {
        var list = std.ArrayList(Stmt).init(self.allocator);
        errdefer list.deinit();
        try list.appendSlice(&[_]Stmt{
            body.*,
            .{ .expr_stmt = expr },
        });

        body = try self.createStmt(.{ .block = try list.toOwnedSlice() });
    }
    if (condition == null) condition = try self.createExpr(.{
        .literal = .{ .bool = true },
    });
    body = try self.createStmt(.{ .while_stmt = .{
        .condition = condition.?,
        .body = try self.createStmt(body.*),
    } });

    if (initializer) |stmt| {
        var list = std.ArrayList(Stmt).init(self.allocator);
        errdefer list.deinit();

        try list.appendSlice(&[_]Stmt{
            stmt,
            body.*,
        });
        body = try self.createStmt(.{ .block = try list.toOwnedSlice() });
    }

    return body.*;
}

fn breakStatement(self: *Parser) Error!Stmt {
    assert(self.checkPrevious(.@"break"));

    const is_with_label = false;
    const value: ?*Expr = blk: {
        if (comptime is_with_label) {
            break :blk try self.expression();
        } else {
            break :blk null;
        }
    };
    _ = try self.consume(.semicolon, "Expect ';' after 'break'.");

    return .{ .break_stmt = value };
}

fn returnStatement(self: *Parser) Error!Stmt {
    assert(self.checkPrevious(.@"return"));

    const keyword: Token = self.previous();
    const value: ?*Expr = blk: {
        if (!self.check(.semicolon)) {
            break :blk try self.expression();
        } else {
            break :blk null;
        }
    };
    _ = try self.consume(.semicolon, "Expect ';' after return value.");

    const stmt: Stmt = .{ .return_stmt = .{
        .keyword = keyword,
        .value = value,
    } };

    return stmt;
}

fn statement(self: *Parser) Error!Stmt {
    if (self.match(TypeSets.@"if")) return self.ifStatement();
    if (self.match(TypeSets.@"for")) return self.forStatement();
    if (self.match(TypeSets.print)) return self.printStatement();
    if (self.match(TypeSets.@"return")) return self.returnStatement();
    if (self.match(TypeSets.@"while")) return self.whileStatement();
    if (self.match(TypeSets.left_brace)) return .{ .block = try self.block() };
    if (self.match(TypeSets.@"break")) return self.breakStatement();

    return self.expressionStatement(); // finally identifier
}

fn varDeclaration(self: *Parser) Error!Stmt {
    assert(self.checkPrevious(.@"var"));

    const name: Token = try self.consume(.identifier, "Expect variable name.");
    const value: ?*Expr = blk: {
        if (self.match(TypeSets.declaration)) {
            break :blk try self.expression();
        } else {
            break :blk null;
        }
    };
    _ = try self.consume(.semicolon, "Expect ';' after variable declaration.");

    const stmt: Stmt = .{ .var_stmt = .{
        .name = name,
        .initializer = value,
    } };

    return stmt;
}

fn functionDeclaration(self: *Parser, comptime kind: []const u8) Error!Stmt {
    assert(self.checkPrevious(.fun));

    const name: Token = try self.consume(.identifier, "Expect " ++ kind ++ " name.");
    _ = try self.consume(.left_paren, "Expect '(' after " ++ kind ++ " name.");

    var parameters = std.ArrayList(Token).init(self.allocator);
    errdefer parameters.deinit();

    if (!self.check(.right_paren)) {
        var do = true;
        while (do or self.match(TypeSets.comma)) {
            do = false;
            if (parameters.items.len >= 255) {
                return parseError(self.peek(), "Can't have more than 255 parameters.");
            }

            try parameters.append(try self.consume(.identifier, "Expect parameter name."));
        }
    }
    _ = try self.consume(.right_paren, "Expect ')' after parameters.");

    _ = try self.consume(.left_brace, "Expect '{{' before " ++ kind ++ " body.");

    const body: []Stmt = try self.block();
    const params: []Token = try parameters.toOwnedSlice();

    const fun = try Stmt.Function.create(self.allocator);
    fun.* = .{
        .name = name,
        .parameters = params,
        .body = body,
    };

    return .{ .function = fun.* };

    // See https://craftinginterpreters.com/functions.html#function-declarations
}

fn classDeclaration(self: *Parser, comptime kind: []const u8) Error!Stmt {
    assert(self.checkPrevious(.class));

    const name: Token = try self.consume(.identifier, "Expect " ++ kind ++ " name.");
    _ = try self.consume(.left_brace, "Expect '{{' before " ++ kind ++ " body.");

    var methods = std.ArrayList(Stmt.Function).init(self.allocator);
    errdefer methods.deinit();
    while (!self.check(.right_brace) and !self.isAtEnd()) {
        const method: Stmt = try self.functionDeclaration(Stmt.Function.function_kind);
        try methods.append(method.function);
    }

    _ = try self.consume(.right_brace, "Expect '}}' after " ++ kind ++ " body.");
    _ = try self.consume(.semicolon, "Expect ';' after " ++ kind ++ " declaration.");

    const class = try Stmt.Class.create(self.allocator);
    class.* = .{
        .name = name,
        .methods = try methods.toOwnedSlice(),
    };

    return .{ .class = class.* };
}

fn declaration(self: *Parser) Allocator.Error!?Stmt {
    const stmt_result = blk: {
        break :blk if (self.match(TypeSets.class))
            self.classDeclaration(Stmt.Class.class_kind)
        else if (self.match(TypeSets.fun))
            self.functionDeclaration(Stmt.Function.function_kind)
        else if (self.match(TypeSets.@"var"))
            self.varDeclaration()
        else
            self.statement();
    };

    return (stmt_result) catch |err| switch (err) {
        Error.parse_error, Error.io_error => blk: {
            self.synchronize();
            break :blk null;
        },
        else => |other| other,
    };
}

pub fn parseExpression(self: *Parser) Allocator.Error!?*Expr {
    return self.expression() catch |err| switch (err) {
        Error.parse_error => null,
        Error.io_error => null,
        else => |other| other,
    };
}

pub fn parse(self: *Parser) Allocator.Error![]Stmt {
    var statements = std.ArrayList(Stmt).init(self.allocator);
    errdefer statements.deinit();

    while (!self.isAtEnd()) {
        if (try self.declaration()) |decl| {
            try statements.append(decl);
        }
    }

    return try statements.toOwnedSlice();
}

// See prerequisites: https://craftinginterpreters.com/representing-code.html
//
// See also: https://craftinginterpreters.com/parsing-expressions.html#recursive-descent-parsing
// It’s called “recursive descent” because it walks down the grammar.
// Confusingly, we also use direction metaphorically when talking about “high”
// and “low” precedence, but the orientation is reversed. In a top-down parser,
// you reach the lowest-precedence expressions first because they may in turn
// contain subexpressions of higher precedence.
//
//    TOP                          LOWER
//     ^                            ^
//     |         Equality           |
//     |         Comparison         |
//    GRAMMER    Addition          PRECEDENCE
//     |         Multiplication     |
//     |         Unary              |
//     v                            v
//    BOTTOM                       HIGHER
//
//
// The descent is described as “recursive” because when a grammar rule refers
// to itself—directly or indirectly—that translates to a recursive function
// call.
//
//
// | Grammar notation | Code representation               |
// | ---------------- | --------------------------------- |
// | Terminal         | Code to match and consume a token |
// | Nonterminal      | Call to that rule's function      |
// | `\|`             | `if` or `switch` statement        |
// | `*` or `+`       | `while` or `for` loop             |
// | `?`              | `if` statement                    |
//
//
// Precedence Level
//
//     expression     → equality ;
//     equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//     comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//     term           → factor ( ( "-" | "+" ) factor )* ;
//     factor         → unary ( ( "/" | "*" ) unary )* ;
//     unary          → ( "!" | "-" ) unary
//                    | primary ;
//     primary        → NUMBER | STRING | "true" | "false" | "nil"
//                      | "(" expression ")" ;
//
// see also: https://craftinginterpreters.com/representing-code.html#a-grammar-for-lox-expressions
//
// Since the grammar is recursive—note how grouping, unary, and binary all
// refer back to expression—our data structure will form a tree. Since this
// structure represents the syntax of our language, it’s called a syntax tree.
//
//
// see https://craftinginterpreters.com/statements-and-state.html#block-syntax-and-semantics
//
//      statement      → exprStmt
//                      | printStmt
//                      | block ;
//
//      block          → "{" declaration* "}" ;

// n.b
//
// A MultiArrayList stores a list of a struct or tagged union type.
// Instead of storing a single list of items, MultiArrayList stores
// separate lists for each field of the struct or lists of tags and bare
// unions.
// This allows for memory savings if the struct or union has padding, and
// obtain a slice of field values.

// Docs

// parse()
// The caller owns the returned memory. Empties this ArrayList, Its
// capacity is cleared, making deinit() safe but unnecessary to call.

// declaration()
// See https://github.com/jwmerrill/zig-lox/blob/main/src/compiler.zig#L326

// varDeclaration()
// Requires and consumes an identifier token for the variable name; and
// handles either assignment or no assignment.

// statement()
// If the next token doesn’t look like any known kind of statement, we
// assume it must be an expression statement. final fallthrough case, since
// it’s hard to proactively recognize an expression from its first token.

// breakStatement()
// See https://gitlab.com/andreyorst/lox/-/blob/main/src/clojure/lox/parser.clj?ref_type=heads
//
// (defn- break-statement [tokens n]
//   [(Break. (current tokens n))
//     (consume tokens (inc n) :semicolon "Expect ';' after break.")])
//
// At runtime, a break statement causes execution to jump to the end of the
// nearest enclosing loop and proceeds from there. Note that the break may
// be nested inside other blocks and if statements that also need to be
// exited.

// forStatement()
// The for loop syntax provides a convenient way to structure our code, but
// it doesn't introduce any new capabilities beyond what we can achieve
// with existing statements. If the for loop didn't have initializer
// clauses, we could simply declare our variables before the loop.
// Similarly, without an increment clause, we could handle that at the end
// of the loop body ourselves.

// ifStatement()
// createStmt uses the allocator to allocate memory on the heap, avoiding
// stack lifetime issues. By dynamically allocating the then_branch, we
// avoid referencing memory that would be invalidated after the function
// returns. The then_branch pointer now refers to memory that persists
// beyond the function scope, ensuring that it's valid when later
// referenced in the IfStmt.
// See also https://craftinginterpreters.com/control-flow.html

// block()
// nb. Having block() return the raw list of statements and leaving it to
// statement() to wrap the list in a Stmt.Block looks a little odd. I did it
// that way because we’ll reuse block() later for parsing function bodies and
// we don’t want that body wrapped in a Stmt.Block.

// synchronize()
// In a recursive descent parser, state is managed by the call stack. To
// reset during synchronization, `ParseError` error are thrown and caught at
// statement boundaries. The parser discards tokens until a likely statement
// boundary (semicolon or keyword) is reached, preventing cascaded errors and
// allowing parsing to continue.

// expression()
// The difference is that the left-hand side of an assignment isn’t an
// expression that evaluates to a value. It’s a sort of
// pseudo-expression that evaluates to a “thing” you can assign to.

// assignment()
// Enhances single token of lookahead similar to binary operators like `+`.
//
// Consider:
//
//   var a = "before";
//   a = "value";
//
// On the second line, we don’t evaluate a (which would return the
// string “before”). We figure out what variable a refers to so we know
// where to store the right-hand side expression’s value. The classic
// terms for these two constructs are l-value and r-value. All of the
// expressions that we’ve seen so far that produce values are r-values.
// An l-value “evaluates” to a storage location that you can assign
// into.
// In fact, the names come from assignment expressions: l-values appear
// on the left side of the = in an assignment, and r-values on the right.
//
// Parse "lhs", which can be any expression of higher precedence.
// previously: try self.equality();
//
// When we find an `=`, we parse right-hand side, and then wrap it all
// up in an assignment expression tree node. Since assignment is
// right-associative, recursively call 'assignment()' to parse "rhs".
//
// The trick is to look at "lhs" expression and figure out what kind of
// assignment target it is - **just before we create** the assignment
// expression node!
//
// This conversion works as every valid assignment target happens
// to also be valid syntax as a normal expression.
//
// We report an error if the left-hand side isn’t a valid
// assignment target, but we don’t throw it because the parser
// isn’t in a confused state where we need to go into panic mode
// and synchronize. That ensures we report an error on code such as
// `a + b = c;`

// orExpr()
// Calls andExpr() (next higher precedence level) for its operands.
// Parsing a series of `or` expressions mirrors other binary operators.

// andExpr()
// Calls equality() for its operands.

// call()
// Similar to how we parse infix operators. First, we parse a primary
// expression, the “left operand” to the call. Then, each time we see a (,
// we call finishCall() to parse the call expression using the previously
// parsed expression as the callee. The returned expression becomes the new
// expr and we loop to see if the result is itself called.

// advance()
// Increments current counter and returns the token if is not at end.
