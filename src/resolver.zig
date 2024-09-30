//! Resolves each variable definition and usage with it's corresponding scope.
//! Stores the scope number in the `locals` which is returned as an artifacto
//! of calling the resolver.

const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;

const Expr = @import("expr.zig").Expr;
const Interpreter = @import("interpreter.zig");
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const Value = @import("expr.zig").Expr.Value;
const debug = @import("debug.zig");
const logger = @import("logger.zig");
const main = @import("main.zig");
const root = @import("root.zig");

/// Keys, as in Environment, are variable names.
/// The values are Booleans, for a reason I’ll explain soon.
pub const BlockScope = std.StringHashMap(bool);
pub const ScopeStack = std.ArrayList(BlockScope);

/// Resolver provides static analysis before running the interpretter.
const Resolver = @This();

allocator: Allocator,
interpreter: *Interpreter,
scopes: ScopeStack,
current_function: FunctionType = FunctionType.none,

comptime {
    assert(@sizeOf(@This()) == 72);
    assert(@alignOf(@This()) == 8);
}

const loggerscoper_beginScope: logger.Scoper = .{ .scope = .{
    .name = "beginScope",
    .parent = null,
} };

const FunctionType = enum {
    none,
    function,
    initializer,
    method,

    pub fn isFn(self: FunctionType) bool {
        return switch (self) {
            .function, .method => true,
            inline else => false,
        };
    }
};

const ClassType = enum {
    none,
    class,
    subclass,
};

const ResolveError = error{
    invalid_return,
    undefined_variable,
    unused_variable,
    variable_already_declared,
};

pub const Error = ResolveError || Allocator.Error;

pub fn handleTokenError(err: Error, token: Token, comptime fmt: []const u8) Allocator.Error!void {
    return switch (err) {
        error.variable_already_declared,
        error.invalid_return,
        error.unused_variable,
        error.undefined_variable,
        => blk: {
            logger.err(.default, @src(), "Handling 'static token error'.{s}:[err: '{}' for token '{s}' ].", //
                .{ logger.newline, err, token });

            break :blk main.tokenError(token, fmt);
        },
        inline else => |other| blk: {
            logger.err(.default, @src(), "Handling errors other than 'static token error'.{s}:[err: '{any}' for token '{s}' ].", //
                .{ logger.newline, other, token });
            break :blk other;
        },
    };
}

pub fn init(allocator: Allocator, interpreter: *Interpreter) Resolver {
    return .{
        .allocator = allocator,
        .interpreter = interpreter,
        .scopes = ScopeStack.init(allocator),
    };
}

pub fn deinit(self: *Resolver) void {
    _ = self;

    std.log.warn("Tried to deinit unimplemented Resolver.deinit()", .{});
}

fn scopesSize(self: *const Resolver) usize {
    return self.scopes.items.len;
}

fn isEmptyScopeStack(self: *const Resolver) bool {
    return (self.scopesSize() == 0);
}

// Since item is in scopes memory we return the pointer to it.
// NOTE: can also use `&self.scopes.getLastOrNull()`
fn getLastOrNullScopeStackPtr(self: *Resolver) ?*BlockScope {
    if (!self.isEmptyScopeStack()) {
        return &self.scopes.items[self.scopesSize() - 1];
    } else {
        return null;
    }
}

fn getLastScopeStackPtr(self: *Resolver) *BlockScope {
    assert(!self.isEmptyScopeStack());

    return self.getLastOrNullScopeStackPtr().?;
}

fn beginScope(self: *Resolver) Allocator.Error!void {
    var prev_size: usize = undefined;
    if (comptime debug.is_trace_resolver) {
        prev_size = self.scopes.items.len;
    }

    const scope = BlockScope.init(self.allocator);
    if (comptime debug.is_trace_resolver) {
        logger.info(.{ .scope = loggerscoper_beginScope.scope }, @src(),
            \\Pushing scope.{s}[prev_size: {d}]
        , .{ logger.newline, prev_size });
    }
    try self.scopes.append(scope);
}

fn endScope(self: *Resolver) void {
    const scoper: logger.Scoper = .{ .scope = .{ .name = @src().fn_name, .parent = &loggerscoper_beginScope.scope } };
    const scope: BlockScope = self.scopes.pop(); // note: pop invalidates element pointers to the removed element

    logger.info(
        scoper,
        @src(),
        "Finally popped scope!{s}[Metadata: {any}]{s}[Scopes count: {any}]",
        .{ logger.newline, scope.unmanaged.metadata, logger.newline, scope.unmanaged.count() },
    );

    var it = scope.iterator();
    while (it.next()) |entry| {
        const is_used: bool = entry.value_ptr.*;
        const name: []const u8 = entry.key_ptr.*;
        const is_not_used = (!is_used and
            !mem.startsWith(u8, name, "_") and
            !mem.eql(u8, name, "this") and
            !mem.eql(u8, name, "super"));
        if (is_not_used) {
            const token = undefined;
            handleTokenError(
                Error.unused_variable,
                token,
                "Unused local variable. Start variable name with underscore if variable is unused",
            ) catch @panic("Unimplemented");
            @panic("Unimplemented");
        }
    }
}

fn declare(self: *Resolver, name: Token) Allocator.Error!void {
    const scoper: logger.Scoper = .{ .scope = .{
        .name = @src().fn_name,
        .parent = if (self.current_function.isFn()) &loggerscoper_beginScope.scope else null,
    } };

    if (self.isEmptyScopeStack()) { // FIXME: How should we declare a function variable name if it is global. since, we return immediately?
        assert(!self.current_function.isFn());
        return;
    }

    if (self.scopes.items[self.scopesSize() - 1].contains(name.lexeme)) {
        logger.err(scoper, @src(), "Already a variable with this name in this scope.{s}[name: {any} ]{s}[peeked scope item count: {d}]", //
            .{ logger.newline, name, logger.newline, self.scopes.items[self.scopesSize() - 1].count() });
        try handleTokenError(Error.variable_already_declared, name, "Already a variable with this name in this scope.");
    }

    const is_resolved = false;
    try self.scopes.items[self.scopesSize() - 1].put(
        name.lexeme,
        is_resolved,
    );

    logger.info(scoper, @src(),
        \\"Token declared. {s}[peeked scope item count: {d}]"
    , .{ logger.newline, self.scopes.items[self.scopesSize() - 1].count() });
}

fn define(self: *Resolver, name: Token) Error!void {
    if (self.isEmptyScopeStack()) {
        return logger.info(.default, @src(), "Found empty stack.{s}[name: {any}]", .{ logger.newline, name });
    }

    const is_resolved = true; //> is fully initialized and ready for use
    try self.getLastOrNullScopeStackPtr().?.put(name.lexeme, is_resolved);
}

fn resolveLocal(self: *Resolver, expr: *Expr, name: Token) Error!void {
    if (self.isEmptyScopeStack()) return; // expect local to be in global scope
    if (self.current_function.isFn()) assert(self.scopesSize() >= 1); // expect function to be scoped

    const scope_index = @as(i32, @intCast(self.scopesSize())) - 1;
    assert(scope_index >= 0); // expect beginScope() to be called before resolveLocal

    const scoper = logger.Scoper.makeScope(@src()).withParent(&loggerscoper_beginScope.scope);

    // see https://craftinginterpreters.com/resolving-and-binding.html#resolving-variable-expressions
    var i: i32 = scope_index;
    while (i >= 0) : (i -= 1) {
        logger.info(scoper, @src(),
            \\Resolving local variable.{s}name: '{any}' at scope index '{d}'..
        , .{ logger.newline, name, i });

        if (self.scopes.items[@intCast(i)].contains(name.lexeme)) {
            assert((scope_index >= i) and (scope_index - i >= 0));
            logger.info(scoper, @src(),
                \\Resolving interpreter's locals.{s}Current local is '{s}'.
            , .{ logger.newline, name.lexeme });

            try self.interpreter.resolve(@constCast(expr), name, scope_index - i);
            assert(self.interpreter.locals.contains(name.lexeme)); // sanity check

            return;
        }
    }
}

fn resolveFunction(
    self: *Resolver,
    function: *const Stmt.Function,
    @"type": FunctionType,
) Error!void {
    const prev_size: usize = self.scopesSize();
    const enclosing_function: FunctionType = self.current_function;
    self.current_function = @"type";
    defer self.current_function = enclosing_function;

    try self.beginScope();
    {
        assert(prev_size < self.scopesSize());
        for (function.parameters) |parameter| {
            try self.declare(parameter);
            try self.define(parameter);
        }
        try self.resolveStatements(function.body);
        assert(self.scopesSize() >= 0);
    }
    self.endScope();
}

pub fn resolveStatements(self: *Resolver, statements: []const Stmt) Allocator.Error!void {
    for (statements) |*stmt| {
        resolveStatement(self, stmt) catch |err| {
            const token_ptr = root.unionPayloadPtr(Token, stmt) orelse unreachable;
            logger.err(.default, @src(), "Found payload ptr.{s}[stmt: {any}, token: {any}]", //
                .{ logger.newline, stmt, token_ptr });

            return try handleTokenError(err, token_ptr.*, "Failed to resolve statement.");
        };
    }
}

pub fn resolveStatement(self: *Resolver, stmt: *const Stmt) Error!void {
    const scoper: logger.Scoper = .{ .scope = .{ .name = @src().fn_name } };

    switch (stmt.*) {
        .block => |statements| {
            try self.beginScope();
            try self.resolveStatements(statements);
            self.endScope();
        },
        .break_stmt => |_| {
            @panic("Unimplemented");
        },
        .expr_stmt => |expr_stmt| {
            try self.resolveExpr(expr_stmt);
        },
        .function => |function| {
            logger.info(scoper, @src(),
                \\Found function initializer.{s}[function: '{s}']"
            , .{ logger.newline, function.name });
            try self.declare(function.name);
            try self.define(function.name);
            try self.resolveFunction(&function, FunctionType.function);
        },
        .if_stmt => |if_stmt| {
            try self.resolveExpr(if_stmt.condition);
            try self.resolveStatement(if_stmt.then_branch);
            if (if_stmt.else_branch) |else_branch| {
                try self.resolveStatement(else_branch);
            }
        },
        .print_stmt => |print_stmt| {
            try self.resolveExpr(print_stmt);
        },
        .return_stmt => |return_stmt| {
            if (self.current_function == .none) {
                return handleTokenError(
                    Error.invalid_return,
                    return_stmt.keyword,
                    "Can't return from top level code.",
                );
            }
            if (return_stmt.value) |expr| {
                try self.resolveExpr(expr);
            }
        },
        .var_stmt => |var_stmt| {
            // Resolving a variable declaration adds a new entry to the current
            // innermost scope’s map.
            try self.declare(var_stmt.name);
            if (var_stmt.initializer) |expr| {
                logger.info(scoper, @src(), "Found variable initializer.{s}[expr: '{any}']", //
                    .{ logger.newline, expr });
                try self.resolveExpr(expr);
            }
            try self.define(var_stmt.name);
        },
        .while_stmt => |while_stmt| {
            try self.resolveExpr(while_stmt.condition);
            try self.resolveStatement(while_stmt.body);
        },
    }
}

pub fn resolveExpr(self: *Resolver, expr: *Expr) Error!void {
    switch (expr.*) {
        .assign => |assign| {
            try self.resolveExpr(assign.value);
            // This resolves reference to other variables.
            try self.resolveLocal(assign.value, assign.name);
        },
        .binary => |binary| {
            try self.resolveExpr(binary.left);
            try self.resolveExpr(binary.right);
        },
        .call => |call| {
            try self.resolveExpr(call.callee);
            for (call.arguments) |arg_expr| {
                try self.resolveExpr(arg_expr);
            }
        },
        .grouping => |grouping| {
            try self.resolveExpr(grouping);
        },
        .literal => |_| {
            // A literal expression doesn’t mention any variables and doesn’t
            // contain any subexpressions so there is no work to do.
        },
        .logical => |logical| {
            // Static analysis does no control flow or short-circuiting.
            try self.resolveExpr(logical.left);
            try self.resolveExpr(logical.right);
        },
        .unary => |unary| {
            try self.resolveExpr(unary.right);
        },
        .variable => |variable| {
            const name: []const u8 = variable.lexeme;
            if (!self.isEmptyScopeStack()) {
                if (self.getLastScopeStackPtr().get(name)) |is_declared| {
                    if (!is_declared) {
                        return handleTokenError(
                            Error.undefined_variable,
                            variable,
                            "Can't read local variable in its own initializer.",
                        );
                    }
                }
            }
            logger.info(loggerscoper_beginScope, @src(), "Resolving variable initializer expression.{s}[variable: '{any}'].", //
                .{ logger.newline, variable });
            try self.resolveLocal(expr, variable);
        },
    }
}

// Why is this here ???????????????????????????????
pub fn resolveExpression(allocator: Allocator, interpreter: *Interpreter, expr: *Expr) Error!std.AutoHashMap(*Expr, usize) {
    const resolver: Resolver = try Resolver.init(allocator, interpreter);
    defer resolver.deinit();

    try resolver.resolveExpr(expr);

    return resolver.interpreter.locals;
}

//
//
// TESTS
//
//
//

const TestContext = struct {
    allocator: Allocator,
    interpreter: Interpreter,
    resolver: *Resolver,
    statements: std.ArrayList(Stmt),

    pub fn init(allocator: Allocator) !TestContext {
        const self = try allocator.create(TestContext);
        errdefer allocator.destroy(self);

        self.* = TestContext{
            .allocator = allocator,
            .interpreter = try Interpreter.init(allocator),
            .resolver = try allocator.create(Resolver),
            .statements = std.ArrayList(Stmt).init(allocator),
        };
        self.resolver.* = Resolver.init(self.allocator, &self.interpreter);

        return self.*;
    }

    pub fn deinit(self: *TestContext) void {
        self.statements.deinit();
        self.allocator.destroy(self.resolver);
        self.allocator.destroy(self);
    }
};

test "invalid top-level return" {
    const skip_test = false;

    if (comptime !skip_test) {
        // Reset to default before each test.
        {
            main.g_had_error = false;
            main.g_error_count = 0;
            main.g_had_runtime_error = false;
            main.g_runtime_error_count = 0;
        }

        // Reset to default after each test.
        defer {
            main.g_had_error = false;
            main.g_error_count = 0;
            main.g_had_runtime_error = false;
            main.g_runtime_error_count = 0;
        }

        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        var ctx = try TestContext.init(allocator);
        defer ctx.deinit();

        try ctx.statements.appendSlice(&[_]Stmt{
            .{ .return_stmt = .{
                .value = @constCast(&.{ .literal = Value.Nil }),
                .keyword = Token.make("return", 1, .{ .str = "return" }, .identifier),
            } },
        });
        assert(ctx.statements.items.len == 1);

        try ctx.resolver.resolveStatements(try ctx.statements.toOwnedSlice());

        // Check error flags
        try testing.expect(main.g_had_error);
        try testing.expect(main.g_error_count == 1);
        // ...since top level return is in global scope, that is handled
        // directly by the interpreter
        try testing.expect(!main.g_had_runtime_error);
        try testing.expect(main.g_runtime_error_count == 0);

        // Check runtime values
        // TODO: Why does `BANG  null fail`, and `MINUS - null` pass?
        try testing.expectEqualStrings("false", try std.fmt.allocPrint(allocator, "{any}", .{Interpreter.runtime_return_value}));
        try testing.expectEqualStrings("error.", try std.fmt.allocPrint(allocator, "{any}", .{Interpreter.runtime_error}));

        // NOTE: Expect error
        //      [default] src/resolver.zig:handleTokenError__anon_7330:52:29: err: Handling static token error.
        //      └─ :[err: 'error.invalid_return' for token 'IDENTIFIER return return' ].
        //      [line 1] Error at 'return': Can't return from top level code.
    }
}

test "variable declaration" {
    const skip_test = false;

    if (comptime !skip_test) {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        var ctx = try TestContext.init(allocator);
        defer ctx.deinit();

        // Test variable declaration
        try ctx.statements.appendSlice(&[_]Stmt{.{
            .var_stmt = .{
                .name = Token.make("x", 1, .{ .str = "x" }, .identifier),
                .initializer = null,
            },
        }});

        // Test variable usage ─ Shadow declaration
        const var_expr = &Expr{ .variable = Token.make("x", 2, .{ .str = "x" }, .identifier) };
        try ctx.statements.appendSlice(&[_]Stmt{.{ .expr_stmt = @constCast(var_expr) }});
        try ctx.resolver.resolveStatements(try ctx.statements.toOwnedSlice());

        // FIXME: I don't understand what the resolution isl
        {
            // Assert that the variable was properly resolved
            try testing.expect(!ctx.interpreter.environment.values.contains(var_expr.variable.lexeme));
            try testing.expect(!ctx.interpreter.globals.values.contains(var_expr.variable.lexeme));
            try testing.expect(!ctx.interpreter.locals.contains(var_expr.variable.lexeme));
        }
    }
}

test "stack" {
    const test_allocator = testing.allocator;

    const string = "(()())";
    var stack = std.ArrayList(usize).init(
        test_allocator,
    );
    defer stack.deinit();

    const Pair = struct { open: usize, close: usize };
    var pairs = std.ArrayList(Pair).init(
        test_allocator,
    );
    defer pairs.deinit();

    for (string, 0..) |char, i| {
        if (char == '(') try stack.append(i);
        if (char == ')')
            try pairs.append(.{
                .open = stack.pop(),
                .close = i,
            });
    }

    for (pairs.items, 0..) |pair, i| {
        try testing.expect(std.meta.eql(pair, switch (i) {
            0 => Pair{ .open = 1, .close = 2 },
            1 => Pair{ .open = 3, .close = 4 },
            2 => Pair{ .open = 0, .close = 5 },
            else => unreachable,
        }));
    }
}

//
// Docs
//

// beginScope()
// `std.ArrayList` provides the methods necessary to use it as a stack.
// Lexical scopes nest in both the interpreter and the resolver. They behave
// like a stack. The interpreter implements that stack using a linked list—the
// chain of Environment objects.

// endScope()
// The scope stack is only used for local block scopes. Variables declared at
// the top level in the global scope are not tracked by the resolver since they
// are more dynamic in Lox. When resolving a variable, if we can’t find it in
// the stack of local scopes, we assume it must be global.

// declare()
// Declaration adds the variable to the innermost scope so that it shadows any
// outer one and so that we know the variable exists. We mark it as “not ready
// yet” by binding its name to false in the scope map. The value associated
// with a key in the scope map represents whether or not we have finished
// resolving that variable’s initializer.
//
// When we declare a variable in a local scope, we already know the names
// of every variable previously declared in that same scope. If we see a
// collision, we report an error.

// define()
// After (`declare()`) declaring the variable, we resolve its initializer
// expression in that same scope where the new variable now exists but is
// unavailable.
// Once the initializer expression is done, the variable is ready
// for prime time. We do that by (`define()`) defining it.

// resolveLocal()
// This looks, for good reason, a lot like the code in Environment for
// evaluating a variable. We start at the innermost scope and work outwards,
// looking in each map for a matching name. If we find the variable, we resolve
// it, passing in the number of scopes between the current innermost scope and
// the scope where the variable was found. So, if the variable was found in the
// current scope, we pass in 0. If it’s in the immediately enclosing scope, 1.
//
// If we walk through all of the block scopes and never find the variable,
// we leave it unresolved and assume it’s global. We’ll get to the
// implementation of that resolve() method a little later. For now, let’s
// keep on cranking through the other syntax nodes.

// resolveFunction()
// We need to track not just that we’re in a function, but how many we’re in.
//
// We could use an explicit stack of FunctionType values for that, but
// instead we’ll piggyback on the JVM. We store the previous value in a
// local on the Java stack. When we’re done resolving the function body, we
// restore the field to that value.

// resolveStatement()
// Since the resolver needs to visit every node in the syntax tree, it
// implements the visitor abstraction we already have in place. Only a few
// kinds of nodes are interesting when it comes to resolving variables:
//
// * A block statement introduces a new scope for the statements it contains.
// * A function declaration introduces a new scope for its body and binds its
//   parameters in that scope.
// * A variable declaration adds a new variable to the current scope.
// * Variable and assignment expressions need to have their variables resolved.
//
// The rest of the nodes don’t do anything special, but we still need to
// implement visit methods for them that traverse into their subtrees. Even
// though a + expression doesn’t itself have any variables to resolve, either
// of its operands might.
//
// See also:
// * https://craftinginterpreters.com/resolving-and-binding.html#a-resolver-class
// * https://gitlab.com/andreyorst/lox/-/blob/main/src/clojure/lox/resolver.clj?ref_type=heads

// resolveExpr()
// These methods are similar to the evaluate() and execute() methods in
// Interpreter—they turn around and apply the Visitor pattern to the given
// syntax tree node.

// resolveLocal()
// NOTE: This looks, for good reason, a lot like the code in Environment for
// evaluating a variable. We start at the innermost scope and work outwards,
// looking in each map for a matching name. If we find the variable, we resolve
// it, passing in the number of scopes between the current innermost scope and
// the scope where the variable was found. So, if the variable was found in the
// current scope, we pass in 0. If it’s in the immediately enclosing scope, 1.
// You get the idea.
// If we walk through all of the block scopes and never find the variable, we
// leave it unresolved and assume it’s global.
//
// TODO:
//      In the resolver, allow unresolved variables to pass silently or log them
//      when you're confident they should be global.
//      The interpreter (via lookupVariable) will handle unresolved variables at
//      runtime, first checking locals, then globals.
//
//      Adjustments to the resolver:
//
//      You can check for the presence of a variable in global scope during the
//      resolve phase but do not enforce an error for unresolved globals.
//      Ensure that during interpretation, only unresolved local variables cause an
//      error.
