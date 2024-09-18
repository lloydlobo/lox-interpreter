const std = @import("std");
const root = @import("root.zig");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;

const AstPrinter = @import("astprinter.zig").AstPrinter;
const Expr = @import("expr.zig").Expr;
const Scanner = @import("scanner.zig").Scanner;
const Stmt = @import("stmt.zig").Stmt;
const If = Stmt.If;
const Token = @import("token.zig").Token;
const tokenError = @import("main.zig").tokenError;

pub const Parser = struct {
    allocator: Allocator,
    current: usize = 0, // index
    tokens: std.MultiArrayList(Token), // []const Token is deprecated
    tokens_len: usize, // count of tokens

    const Error = error{ParseError} || Allocator.Error;

    pub fn init(tokens: []const Token, allocator: Allocator) Allocator.Error!Parser {
        const tokens_len = tokens.len;

        var list = std.MultiArrayList(Token){};
        try list.ensureTotalCapacity(allocator, tokens_len);
        for (tokens) |token| list.appendAssumeCapacity(token);
        assert(list.len == tokens_len);

        return .{
            .tokens = list,
            .tokens_len = tokens_len,
            .allocator = allocator,
        };
    }

    /// The caller owns the returned memory. Empties this ArrayList, Its
    /// capacity is cleared, making deinit() safe but unnecessary to call.
    pub fn parse(self: *Parser) Allocator.Error![]Stmt {
        var statements = std.ArrayList(Stmt).init(self.allocator);
        while (!self.isAtEnd()) {
            if (try self.declaration()) |decl|
                try statements.append(decl);
        }

        return try statements.toOwnedSlice();
    }

    pub fn parseExpression(self: *Parser) Allocator.Error!?*Expr {
        return self.expression() catch |err| switch (err) {
            error.ParseError => null,
            else => |other| other,
        };
    }

    // should s(statement) be union?
    fn createStmt(self: *Parser, value: Stmt) Allocator.Error!*Stmt {
        const stmt = try self.allocator.create(Stmt);
        errdefer self.allocator.destroy(stmt);

        stmt.* = value;
        return stmt;
    }

    fn createExpr(self: *Parser, value: Expr) Allocator.Error!*Expr {
        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);

        expr.* = value;
        return expr;
    }

    fn declaration(self: *Parser) Allocator.Error!?Stmt {
        const stmt_result = if (self.match(.{.@"var"})) self.varDeclaration() else self.statement();

        return (stmt_result) catch |err| switch (err) {
            error.ParseError => blk: {
                self.synchronize();
                break :blk null;
            },
            else => |other| other,
        };
    }

    /// Requires and consumes an identifier token for the variable name; and handles either assignment or no assignment.
    fn varDeclaration(self: *Parser) Error!Stmt {
        const name: Token = try self.consume(.identifier, "Expect variable name.");
        const initializer: ?*Expr = if (self.match(.{.equal})) try self.expression() else null;
        _ = try self.consume(.semicolon, "Expect ';' after variable declaration.");
        return .{ .var_stmt = .{ .name = name, .initializer = initializer } };
    }

    // if the next token doesn’t look like any known kind of statement, we
    // assume it must be an expression statement. final fallthrough case, since
    // it’s hard to proactively recognize an expression from its first token.
    fn statement(self: *Parser) Error!Stmt {
        if (self.match(.{.@"if"})) return self.ifStatement();
        if (self.match(.{.print})) return self.printStatement();
        if (self.match(.{.left_brace})) return .{ .block = (try self.block()) };
        return self.expressionStatement();
    }

    // createStmt uses the allocator to allocate memory on the heap, avoiding
    // stack lifetime issues. By dynamically allocating the then_branch, we
    // avoid referencing memory that would be invalidated after the function
    // returns. The then_branch pointer now refers to memory that persists
    // beyond the function scope, ensuring that it's valid when later
    // referenced in the IfStmt.
    // See also https://craftinginterpreters.com/control-flow.html
    fn ifStatement(self: *Parser) Error!Stmt {
        _ = try self.consume(.left_paren, "Expect '(' after 'if'.");
        const condition: *Expr = try self.expression();
        _ = try self.consume(.right_paren, "Expect ')' after if condition.");

        return (try self.createStmt(.{ .if_stmt = If{
            .condition = condition,
            .then_branch = try self.createStmt(try self.statement()),
            .else_branch = if (self.match(.{.@"else"})) try self.createStmt(try self.statement()) else null,
        } })).*;
    }

    fn printStatement(self: *Parser) Error!Stmt {
        const value: *Expr = try self.expression(); // literal value
        // responsible for setting error flags if any parse errors
        _ = try self.consume(.semicolon, "Expect ';' after value.");

        return .{ .print = value };
    }

    fn expressionStatement(self: *Parser) Error!Stmt {
        const value: *Expr = try self.expression(); //> (*Expr) union(enum)
        _ = try self.consume(.semicolon, "Expect ';' after expression.");

        return .{ .expr = value };
    }

    fn block(self: *Parser) Error![]Stmt {
        var statements = std.ArrayList(Stmt).init(self.allocator);
        // Avoid inifinite loops with `isAtEnd()` if parsing invalid code.
        // If forgot a closing `}`, the parser needs to not get stuck.
        while ((!self.check(.right_brace)) and !self.isAtEnd()) {
            if ((try self.declaration())) |decl|
                try statements.append(decl);
        }
        _ = try self.consume(.right_brace, "Expect '}}' after block."); // note: Using `}}` to escape } used by Zig for formating arguments

        // nb. Having block() return the raw list of statements and leaving it to
        // statement() to wrap the list in a Stmt.Block looks a little odd. I did
        // it that way because we’ll reuse block() later for parsing function
        // bodies and we don’t want that body wrapped in a Stmt.Block.
        return try statements.toOwnedSlice();
    }

    // Enhances single token of lookahead similar to binary operators like `+`.
    fn assignment(self: *Parser) Error!*Expr {
        // Parse "lhs", which can be any expression of higher precedence.
        // previously:
        //     try self.equality();
        const expr: *Expr = try self.orExpr();

        // When we find an `=`, we parse right-hand side, and then wrap it all
        // up in an assignment expression tree node. Since assignment is
        // right-associative, recursively call 'assignment()' to parse "rhs".
        if (self.match(.{.equal})) {
            const equals = self.previous();
            const value = try self.assignment();

            // The trick is to look at "lhs" expression and figure out what kind of assignment target
            // it is - **just before we create** the assignment expression node!
            return switch (expr.*) {
                // This conversion works as every valid assignment target happens to also be valid syntax as a normal expression.
                .variable => |name| try self.createExpr(.{ .assign = .{ .name = name, .value = value } }),

                // We report an error if the left-hand side isn’t a valid assignment target, but we don’t
                // throw it because the parser isn’t in a confused state where we need to go into panic mode
                // and synchronize. That ensures we report an error on code such as `a + b = c;`
                //
                // TODO: Add fields later to support:
                // `a = 3;`    // OK.
                // `(a) = 3;`  // Error.
                else => parseError(equals, "Invalid assignment target."),
            };
        }

        return expr;
    }

    /// Calls andExpr() (next higher precedence level) for its operands.
    /// Parsing a series of `or` expressions mirrors other binary operators.
    fn orExpr(self: *Parser) Error!*Expr {
        var expr: *Expr = try self.andExpr();

        while (self.match(.{.@"or"})) {
            const operator = self.previous();
            const right = try self.andExpr();
            expr = try self.createExpr(.{ .logical = .{ .left = expr, .operator = operator, .right = right } });
        }

        return expr;
    }

    /// Calls equality() for its operands.
    fn andExpr(self: *Parser) Error!*Expr {
        var expr: *Expr = try self.equality();

        while (self.match(.{.@"and"})) {
            const operator = self.previous();
            const right = try self.equality();
            expr = try self.createExpr(.{ .logical = .{ .left = expr, .operator = operator, .right = right } });
        }

        return expr;
    }

    // Precedence Level:
    //
    //     expression → equality → comparison → term → factor → unary → primary

    /// expression → equality ;
    fn expression(self: *Parser) Error!*Expr {
        // The difference is that the left-hand side of an assignment isn’t an
        // expression that evaluates to a value. It’s a sort of
        // pseudo-expression that evaluates to a “thing” you can assign to.
        // Consider:
        //
        // var a = "before";
        // a = "value";
        // On the second line, we don’t evaluate a (which would return the
        // string “before”). We figure out what variable a refers to so we know
        // where to store the right-hand side expression’s value. The classic
        // terms for these two constructs are l-value and r-value. All of the
        // expressions that we’ve seen so far that produce values are r-values.
        // An l-value “evaluates” to a storage location that you can assign
        // into.
        //
        // In fact, the names come from assignment expressions: l-values appear
        // on the left side of the = in an assignment, and r-values on the
        // right.
        //
        return try self.assignment();
    }

    /// equality → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(self: *Parser) Error!*Expr {
        var expr = try self.comparison();

        while (self.match(.{ .bang_equal, .equal_equal })) {
            const operator = self.previous();
            const right = try self.comparison();
            expr = try self.createExpr(.{ .binary = .{ .left = expr, .operator = operator, .right = right } });
        }

        return expr;
    }

    /// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(self: *Parser) Error!*Expr {
        var expr = try self.term();

        while (self.match(.{ .greater, .greater_equal, .less, .less_equal })) {
            const operator = self.previous();
            const right = try self.term();
            expr = try self.createExpr(.{ .binary = .{ .left = expr, .operator = operator, .right = right } });
        }

        return expr;
    }

    /// term → factor ( ( "-" | "+" ) factor )* ;
    fn term(self: *Parser) Error!*Expr {
        var expr = try self.factor();

        while (self.match(.{ .minus, .plus })) {
            const operator = self.previous();
            const right = try self.factor();
            expr = try self.createExpr(.{ .binary = .{ .left = expr, .operator = operator, .right = right } });
        }

        return expr;
    }

    /// factor → unary ( ( "/" | "*" ) unary )* ;
    fn factor(self: *Parser) Error!*Expr {
        var expr = try self.unary();

        while (self.match(.{ .slash, .star })) {
            const operator = self.previous();
            const right = try self.unary();
            expr = try self.createExpr(.{ .binary = .{ .left = expr, .operator = operator, .right = right } });
        }

        return expr;
    }

    /// unary → ( "!" | "-" ) unary | primary ;
    fn unary(self: *Parser) Error!*Expr {
        if (self.match(.{ .bang, .minus })) {
            const operator = self.previous();
            const right = try self.unary(); // recursion to same precedence level

            return try self.createExpr(.{ .unary = .{ .operator = operator, .right = right } });
        }

        return self.primary();
    }

    /// primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(self: *Parser) Error!*Expr {
        if (self.match(.{.false})) return try self.createExpr(.{ .literal = .{ .bool = false } });
        if (self.match(.{.true})) return try self.createExpr(.{ .literal = .{ .bool = true } });
        if (self.match(.{.nil})) return try self.createExpr(.{ .literal = .{ .nil = {} } });
        if (self.match(.{ .number, .string })) return try self.createExpr(.{ .literal = self.previousValue() });

        // grouping precedence - recursion: primary() |> expression()
        if (self.match(.{.left_paren})) { //`(`
            const expr = try self.expression();
            _ = try self.consume(.right_paren, "Expect ')' after expression."); //`)`
            return try self.createExpr(.{ .grouping = expr });
        }

        // variable precedence: from declaration() |> primary()
        if (self.match(.{.identifier}))
            return try self.createExpr(.{ .variable = self.previous() });

        //
        //
        // FIXME: failing test "Statements & State - Variables: Runtime Errors"
        //
        //
        //
        //
        // [line 1] Error at 'var': Expect expression.
        // Encountered 0 Runtime Error and 1 Syntax Error.
        // make: *** [Makefile:47: parse] Error 65
        //
        //
        //
        //
        return parseError(self.peek(), "Expect expression.");
    }

    fn match(self: *Parser, types: anytype) bool {
        return inline for (std.meta.fields(@TypeOf(types))) |field| {
            if (self.check(@field(types, field.name))) {
                _ = self.advance();
                break true;
            }
        } else false;
    }

    fn currentType(self: *const Parser) Token.Type {
        assert(self.current < self.tokens_len);
        return self.tokens.items(.type)[self.current]; // see user: xy1 src/main.zig -> fn current
    }

    fn check(self: *Parser, @"type": Token.Type) bool {
        return switch (self.currentType()) {
            .eof => false, // if is at end
            else => |x| (x == @"type"),
        };
    }

    fn isAtEnd(self: *Parser) bool {
        return (self.currentType() == .eof);
    }

    /// Increments current counter and returns the token if is not at end.
    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        assert(self.current != 0 and self.current < self.tokens_len);

        return self.previous();
    }

    fn peek(self: *Parser) Token {
        assert(self.current >= 0); // undefined implementaion logic if called when current index is 0
        return self.tokens.get(self.current);
    }

    fn consume(self: *Parser, @"type": Token.Type, comptime message: []const u8) Error!Token {
        return if (self.check(@"type")) self.advance() else parseError(self.peek(), message);
    }

    fn previous(self: *Parser) Token {
        assert(self.current > 0);
        return self.tokens.get(self.current - 1);
    }

    fn previousLiteral(self: *Parser) ?Token.Literal {
        assert(self.current > 0);
        return self.tokens.items(.literal)[self.current - 1];
    }

    fn previousType(self: *Parser) Token.Type {
        assert(self.current > 0);
        return self.tokens.items(.type)[self.current - 1];
    }

    fn previousValue(self: *Parser) Expr.Value {
        return if (self.previousLiteral()) |literal| switch (literal) {
            .str => |val| .{ .str = val },
            .num => |val| .{ .num = val },
        } else unreachable;
    }

    /// In a recursive descent parser, state is managed by the call stack. To reset
    /// during synchronization, `ParseError` error are thrown and caught at
    /// statement boundaries. The parser discards tokens until a likely statement
    /// boundary (semicolon or keyword) is reached, preventing cascaded errors and
    /// allowing parsing to continue.
    fn synchronize(self: *Parser) void {
        _ = self.advance();
        while (!self.isAtEnd()) {
            if (self.previousType() == .semicolon) return;
            switch (self.currentType()) {
                .class, .fun, .@"var", .@"for", .@"if", .@"while", .print, .@"return" => return,
                else => {},
            }
            _ = self.advance();
        }
    }

    fn parseError(token: Token, comptime message: []const u8) Error {
        tokenError(token, message);
        return error.ParseError;
    }

    // see https://app.codecrafters.io/courses/interpreter/stages/xy1/code-examples
    // const Error = error{ ExpectedExpression, ExpectedBinaryOperator, ExpectedSemi, UnexpectedToken, UnexpectedEof, } || Allocator.Error;
};

//see prerequisites: https://craftinginterpreters.com/representing-code.html
//
//see also: https://craftinginterpreters.com/parsing-expressions.html#recursive-descent-parsing
//
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
// also improves cache usage if only some fields or just tags are needed
// for a computation.  The primary API for accessing fields is the
// `slice()` function, which computes the start pointers for the array of
// each field.  From the slice you can call `.items(.<field_name>)` to
// obtain a slice of field values.
// For unions you can call `.items(.tags)` or `.items(.data)`.
//
//   pub fn MultiArrayList(comptime T: type) type
