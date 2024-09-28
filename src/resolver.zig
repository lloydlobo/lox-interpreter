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
const logger = @import("logger.zig");
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const Value = @import("expr.zig").Expr.Value;
const debug = @import("debug.zig");
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

const loggerscope_beginScope = logger.Scope{ .name = "beginScope", .parent = null };

const ResolveError = error{
    // invalid_super,
    // invalid_this,
    // self_inheritance,
    variable_already_declared,
    invalid_return,
    undefined_variable,
};
pub const Error = ResolveError || Allocator.Error;

pub fn handleTokenError(err: Error, token: Token, comptime fmt: []const u8) Allocator.Error!void {
    return switch (err) {
        error.variable_already_declared,
        error.invalid_return,
        error.undefined_variable,
        => blk: {
            logger.err(.{}, @src(), "Handling static token error.{s}:[err: '{}' for token '{s}' ].", //
                .{ logger.newline, err, token });
            break :blk main.tokenError(token, fmt);
        },
        inline else => |other| other,
    };
}

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

fn scopesSize(self: *Resolver) usize {
    return self.scopes.items.len;
}

fn isEmptyScopeStack(self: *Resolver) bool {
    return self.scopesSize() == 0;
}

fn getLastOrNullScopeStackPtr(self: *Resolver) ?*BlockScope {
    // Since item is in scopes memory we return the pointer to it.
    // NOTE: can also use `&self.scopes.getLastOrNull()`
    return if (!self.isEmptyScopeStack())
        &self.scopes.items[self.scopesSize() - 1]
    else
        null;
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
        logger.info(loggerscope_beginScope, @src(), "Pushing scope.{s}[prev_size: {d}]", //
            .{ logger.newline, prev_size });
    }

    try self.scopes.append(scope);
}

// The scope stack is only used for local block scopes. Variables declared at
// the top level in the global scope are not tracked by the resolver since they
// are more dynamic in Lox. When resolving a variable, if we can’t find it in
// the stack of local scopes, we assume it must be global.
fn endScope(self: *Resolver) void {
    const logscope: logger.Scope = .{ .name = @src().fn_name, .parent = &loggerscope_beginScope };

    // scopes are stored in an explicit stack
    // Invalidates element pointers to the removed element.
    const scope: BlockScope = self.scopes.pop();

    logger.info(logscope, @src(), "Finally popped scope!{s}[Metadata: {any}]{s}[Scopes count: {any}]", //
        .{ logger.newline, scope.unmanaged.metadata, logger.newline, scope.unmanaged.count() });

    var it = scope.iterator();
    while (it.next()) |entry| {
        const is_used: bool = entry.value_ptr.*;
        const name: []const u8 = entry.key_ptr.*;
        const is_not_used = (!is_used and
            !mem.startsWith(u8, name, "_") and
            !mem.eql(u8, name, "this") and
            !mem.eql(u8, name, "super"));
        if (is_not_used) {
            logger.err(logscope, @src(), "Warning: Unused local variable '{s}'. Start variable name with underscore if variable is unused.\n", //
                .{name});
        }
    }
}

// When we declare a variable in a local scope, we already know the names
// of every variable previously declared in that same scope. If we see a
// collision, we report an error.
fn declare(self: *Resolver, name: Token) Allocator.Error!void {
    const loggerscope_declare: logger.Scope = .{
        .name = @src().fn_name,
        .parent = if (self.current_function.isFn()) &loggerscope_beginScope else null,
    };

    // FIXME: How should we declare a function variable name if it is global.
    // since, we return immediately?
    if (self.isEmptyScopeStack()) {
        logger.info(loggerscope_declare, @src(), "Not a function type. Found empty stack.{s}[name: {any}]", //
            .{ logger.newline, name });
        assert(!self.current_function.isFn());
        return;
    }
    if (self.scopes.items[self.scopesSize() - 1].contains(name.lexeme)) {
        logger.err(loggerscope_declare, @src(), "Already a variable with this name in this scope.{s}[name: {any} ]{s}[peeked scope item count: {d}]", //
            .{ logger.newline, name, logger.newline, self.scopes.items[self.scopesSize() - 1].count() });
        try handleTokenError(Error.variable_already_declared, name, "Already a variable with this name in this scope.");
    }

    const is_resolved = false;
    logger.info(loggerscope_declare, @src(), "Declaring token...{s}[name: {any} ]{s}[peeked scope item count: {d}]", .{ logger.newline, name, logger.newline, self.scopes.items[self.scopesSize() - 1].count() });
    try self.scopes.items[self.scopesSize() - 1].put(name.lexeme, is_resolved);
    logger.info(loggerscope_declare, @src(), "Token declared. {s}[peeked scope item count: {d}]", .{ logger.newline, self.scopes.items[self.scopesSize() - 1].count() });
}

// After (`declare()`) declaring the variable, we resolve its initializer
// expression in that same scope where the new variable now exists but is
// unavailable.
// Once the initializer expression is done, the variable is ready
// for prime time. We do that by (`define()`) defining it.
fn define(self: *Resolver, name: Token) Error!void {
    if (self.isEmptyScopeStack()) {
        return logger.info(.{}, @src(), "Found empty stack.{s}[name: {any}]", .{ logger.newline, name });
    }

    const is_resolved = true; //> is fully initialized and ready for use
    try self.getLastOrNullScopeStackPtr().?.put(name.lexeme, is_resolved);
}

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
fn resolveLocal(self: *Resolver, expr: *Expr, name: Token) Error!void {
    const logscope = logger.Scope{
        .name = "resolveLocal",
        .parent = &loggerscope_beginScope,
    };

    if (self.current_function.isFn()) {
        if (!(self.scopesSize() >= 1)) {
            logger.err(logscope, @src(), "Assertion failed!: `(self.scopesSize() >= 1)`{s}[expr: {any}]{s}[name: {any}]{s}[Actual: {d} > {d}]", //
                .{ logger.newline, expr, logger.newline, name, logger.newline, self.scopesSize(), 0 });
        }
    }

    const scope_index = @as(i32, @intCast(self.scopesSize())) - 1;
    logger.info(logscope, @src(), "[1] Now going to resolve local variable.{s}[name: '{any}' at curr_stack_index: {d}]. ", //
        .{ logger.newline, name, scope_index });

    var i: i32 = scope_index;

    while (i >= 0) : (i -= 1) {
        logger.info(logscope, @src(), "[2] ...and now resolving local variable.{s}[name: '{any}' at stack: {d} ]. ", //
            .{ logger.newline, name, i });

        if (self.scopes.items[@intCast(i)].contains(name.lexeme)) {
            assert((scope_index >= i) and (scope_index - i >= 0));
            logger.info(logscope, @src(), "Interpreter is going to resolve.{s}[lexeme: {s}]", //
                .{ logger.newline, name.lexeme });

            try self.interpreter.resolve(@constCast(expr), name, scope_index - i);
            assert(self.interpreter.simplocals.contains(name.lexeme)); // sanity check
            return;
        }
    }
}

fn resolveFunction(
    self: *Resolver,
    function: *const Stmt.Function,
    @"type": FunctionType,
) Error!void {
    const enclosing_function = self.current_function;
    self.current_function = @"type";
    defer self.current_function = enclosing_function;

    const prev_size = self.scopesSize();

    try self.beginScope();
    assert(prev_size < self.scopesSize());
    for (function.parameters) |parameter| {
        try self.declare(parameter);
        try self.define(parameter);
    }
    try self.resolveStatements(function.body);
    self.endScope();
}

pub fn resolveStatements(self: *Resolver, statements: []const Stmt) Allocator.Error!void {
    for (statements) |*stmt| {
        resolveStatement(self, stmt) catch |err| {
            const token_ptr = root.unionPayloadPtr(Token, stmt) orelse unreachable;
            logger.err(.{}, @src(), "Found payload ptr.{s}[stmt: {any}, token: {any}]", //
                .{ logger.newline, stmt, token_ptr });

            return try handleTokenError(err, token_ptr.*, "Failed to resolve statement.");
        };
    }
}

pub fn resolveStatement(self: *Resolver, stmt: *const Stmt) Error!void {
    const logscope = logger.Scope{ .name = @src().fn_name };

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
            logger.info(logscope, @src(), "Found function initializer.{s}[function: '{s}']", //
                .{ logger.newline, function.name });
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
                return handleTokenError(Error.invalid_return, return_stmt.keyword, "Can't return from top level code.");
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
                logger.info(logscope, @src(), "Found variable initializer.{s}[expr: '{any}']", //
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
    // FIXME:
    //
    // [declare] src/resolver.zig:declare:183:13: warn: Did not call beginScope(). Not a function type. Found empty stack.
    // └─ [name: IDENTIFIER function null]
    //
    //
    switch (expr.*) {
        .assign => |assign| {
            try self.resolveExpr(assign.value);
            // Resolve reference to other variables.
            try self.resolveLocal(assign.value, assign.name);
        },
        .binary => |binary| {
            try self.resolveExpr(binary.left);
            try self.resolveExpr(binary.right);
        },
        .call => |call| {
            try self.resolveExpr(call.callee);
            for (call.arguments) |argument| {
                try self.resolveExpr(argument);
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
            const name = variable.lexeme;
            if (!self.isEmptyScopeStack()) {
                if (self.getLastScopeStackPtr().get(name)) |is_declared| {
                    if (!is_declared) return handleTokenError(Error.undefined_variable, variable, //
                        "Can't read local variable in its own initializer.");
                }
            }
            logger.info(loggerscope_beginScope, @src(), "Resolving variable initializer expression.{s}[variable: '{any}'].", //
                .{ logger.newline, variable });
            try self.resolveLocal(expr, variable);
        },
    }
}

// HOW TO USE THIS ?????????????????????????????
pub fn resolveExpression(
    allocator: Allocator,
    interpreter: *Interpreter,
    expr: *Expr,
) Error!std.AutoHashMap(*Expr, usize) {
    const resolver: Resolver = try Resolver.init(
        allocator,
        interpreter,
    );
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
    try testing.expect(!main.g_had_runtime_error);
    try testing.expect(main.g_error_count == 1);

    // Check runtime values
    try testing.expectEqualStrings("BANG  null", try std.fmt.allocPrint(allocator, "{any}", .{Interpreter.runtime_token}));
    try testing.expectEqualStrings("false", try std.fmt.allocPrint(allocator, "{any}", .{Interpreter.runtime_return_value}));
    try testing.expectEqualStrings("error.", try std.fmt.allocPrint(allocator, "{any}", .{Interpreter.runtime_error}));

    // Expect error
    //      [default] src/resolver.zig:handleTokenError__anon_7330:52:29: err: Handling static token error.
    //      └─ :[err: 'error.invalid_return' for token 'IDENTIFIER return return' ].
    //      [line 1] Error at 'return': Can't return from top level code.
}

test "variable declaration" {
    if (comptime false) {
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

        // Test variable usage
        const var_expr = &Expr{ .variable = Token.make("x", 2, .{ .str = "x" }, .identifier) };
        try ctx.statements.appendSlice(&[_]Stmt{.{ .expr_stmt = @constCast(var_expr) }});
        try ctx.resolver.resolveStatements(try ctx.statements.toOwnedSlice());

        // Assert that the variable was properly resolved
        // try testing.expect(ctx.interpreter.simplocals.contains(@constCast(var_expr)));
        try testing.expect(ctx.interpreter.simplocals.contains(@constCast(var_expr)));
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

// define()
// After declaring the variable, we resolve its initializer expression in that
// same scope where the new variable now exists but is unavailable. Once the
// initializer expression is done, the variable is ready for prime time. We do
// that by defining it.

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

//
// Archive
//

// const ResolveError = error{
//     variable_already_declared,
//     undefined_variable,
//     InvalidThis,
//     InvalidSuper,
//     invalid_return,
//     InvalidBreak,
//     SelfInheritance,
// };
//
// const FunctionType = enum {
//     None,
//     Function,
//     Initializer,
//     Method,
// };
//
// const ClassType = enum {
//     None,
//     Class,
//     Subclass,
// };
//
// const Resolver = struct {
//     allocator: *Allocator,
//     scopes: ArrayList(StringHashMap(bool)),
//     locals: StringHashMap(usize),
//     current_function: FunctionType,
//     current_class: ClassType,
//     loop_depth: usize,
//
//     pub fn init(allocator: *Allocator) Resolver {
//         return .{
//             .allocator = allocator,
//             .scopes = ArrayList(StringHashMap(bool)).init(allocator),
//             .locals = StringHashMap(usize).init(allocator),
//             .current_function = .None,
//             .current_class = .None,
//             .loop_depth = 0,
//         };
//     }
//
//     pub fn deinit(self: *Resolver) void {
//         for (self.scopes.items) |*scope| {
//             scope.deinit();
//         }
//         self.scopes.deinit();
//         self.locals.deinit();
//     }
//
//     fn beginScope(self: *Resolver) !void {
//         try self.scopes.append(StringHashMap(bool).init(self.allocator));
//     }
//
//     fn endScope(self: *Resolver) void {
//         const scope = self.scopes.pop();
//         var it = scope.iterator();
//         while (it.next()) |entry| {
//             const name = entry.key_ptr.*;
//             const used = entry.value_ptr.*;
//             if (!used and !std.mem.startsWith(u8, name, "_") and !std.mem.eql(u8, name, "this") and !std.mem.eql(u8, name, "super")) {
//                 std.debug.print("Warning: Unused local variable '{}'. Start variable name with underscore if variable is unused.\n", .{name});
//             }
//         }
//         scope.deinit();
//     }
//
//     fn declare(self: *Resolver, name: []const u8) !void {
//         if (self.scopes.items.len == 0) return;
//         var scope = &self.scopes.items[self.scopes.items.len - 1];
//         if (scope.contains(name)) {
//             return ResolveError.variable_already_declared;
//         }
//         try scope.put(name, false);
//     }
//
//     fn define(self: *Resolver, name: []const u8) !void {
//         if (self.scopes.items.len == 0) return;
//         var scope = &self.scopes.items[self.scopes.items.len - 1];
//         try scope.put(name, true);
//     }
//
//     fn resolveLocal(self: *Resolver, expr: *const Expr, name: []const u8) !void {
//         var i: usize = self.scopes.items.len;
//         while (i > 0) : (i -= 1) {
//             if (self.scopes.items[i - 1].contains(name)) {
//                 try self.locals.put(@ptrToInt(expr), self.scopes.items.len - i);
//                 return;
//             }
//         }
//     }
//
//     fn resolveFunction(self: *Resolver, func: *const Function, type: FunctionType) !void {
//         const enclosing_function = self.current_function;
//         self.current_function = type;
//
//         try self.beginScope();
//         for (func.params) |param| {
//             try self.declare(param.name);
//             try self.define(param.name);
//         }
//         try self.resolveStatements(func.body);
//         self.endScope();
//
//         self.current_function = enclosing_function;
//     }
//
//     pub fn resolveStatements(self: *Resolver, statements: []const Stmt) !void {
//         for (statements) |stmt| {
//             try self.resolveStatement(stmt);
//         }
//     }
//
//     pub fn resolveStatement(self: *Resolver, stmt: Stmt) !void {
//         switch (stmt) {
//             .Block => |block| {
//                 try self.beginScope();
//                 try self.resolveStatements(block.statements);
//                 self.endScope();
//             },
//             .Var => |var_stmt| {
//                 try self.declare(var_stmt.name.lexeme);
//                 if (var_stmt.initializer) |initializer| {
//                     try self.resolveExpr(initializer);
//                 }
//                 try self.define(var_stmt.name.lexeme);
//             },
//             .Function => |func| {
//                 try self.declare(func.name.lexeme);
//                 try self.define(func.name.lexeme);
//                 try self.resolveFunction(func, .Function);
//             },
//             .Expression => |expr| {
//                 try self.resolveExpr(expr.expression);
//             },
//             .If => |if_stmt| {
//                 try self.resolveExpr(if_stmt.condition);
//                 try self.resolveStatement(if_stmt.then_branch);
//                 if (if_stmt.else_branch) |else_branch| {
//                     try self.resolveStatement(else_branch);
//                 }
//             },
//             .Print => |print_stmt| {
//                 try self.resolveExpr(print_stmt.expression);
//             },
//             .Return => |return_stmt| {
//                 if (self.current_function == .None) {
//                     return ResolveError.invalid_return;
//                 }
//                 if (return_stmt.value) |value| {
//                     if (self.current_function == .Initializer) {
//                         return ResolveError.invalid_return;
//                     }
//                     try self.resolveExpr(value);
//                 }
//             },
//             .While => |while_stmt| {
//                 try self.resolveExpr(while_stmt.condition);
//                 self.loop_depth += 1;
//                 try self.resolveStatement(while_stmt.body);
//                 self.loop_depth -= 1;
//             },
//             .Break => {
//                 if (self.loop_depth == 0) {
//                     return ResolveError.InvalidBreak;
//                 }
//             },
//             .Class => |class_stmt| {
//                 const enclosing_class = self.current_class;
//                 self.current_class = .Class;
//
//                 try self.declare(class_stmt.name.lexeme);
//                 try self.define(class_stmt.name.lexeme);
//
//                 if (class_stmt.superclass) |superclass| {
//                     if (std.mem.eql(u8, class_stmt.name.lexeme, superclass.name.lexeme)) {
//                         return ResolveError.SelfInheritance;
//                     }
//                     self.current_class = .Subclass;
//                     try self.resolveExpr(superclass);
//                     try self.beginScope();
//                     try self.scopes.items[self.scopes.items.len - 1].put("super", true);
//                 }
//
//                 try self.beginScope();
//                 try self.scopes.items[self.scopes.items.len - 1].put("this", true);
//
//                 for (class_stmt.methods) |method| {
//                     const declaration = if (std.mem.eql(u8, method.name.lexeme, "init")) FunctionType.Initializer else FunctionType.Method;
//                     try self.resolveFunction(method, declaration);
//                 }
//
//                 self.endScope();
//
//                 if (class_stmt.superclass != null) self.endScope();
//
//                 self.current_class = enclosing_class;
//             },
//         }
//     }
//
//     pub fn resolveExpr(self: *Resolver, expr: Expr) !void {
//         switch (expr) {
//             .Variable => |var| {
//                 if (self.scopes.items.len > 0) {
//                     const scope = &self.scopes.items[self.scopes.items.len - 1];
//                     if (scope.get(var.name.lexeme)) |declared| {
//                         if (!declared) {
//                             return ResolveError.undefined_variable;
//                         }
//                     }
//                 }
//                 try self.resolveLocal(expr, var.name.lexeme);
//             },
//             .Assign => |assign| {
//                 try self.resolveExpr(assign.value);
//                 try self.resolveLocal(expr, assign.name.lexeme);
//             },
//             .Binary => |binary| {
//                 try self.resolveExpr(binary.left);
//                 try self.resolveExpr(binary.right);
//             },
//             .Call => |call| {
//                 try self.resolveExpr(call.callee);
//                 for (call.arguments) |argument| {
//                     try self.resolveExpr(argument);
//                 }
//             },
//             .Get => |get| {
//                 try self.resolveExpr(get.object);
//             },
//             .Grouping => |grouping| {
//                 try self.resolveExpr(grouping.expression);
//             },
//             .Literal => {},
//             .Logical => |logical| {
//                 try self.resolveExpr(logical.left);
//                 try self.resolveExpr(logical.right);
//             },
//             .Set => |set| {
//                 try self.resolveExpr(set.value);
//                 try self.resolveExpr(set.object);
//             },
//             .Super => |super| {
//                 if (self.current_class == .None) {
//                     return ResolveError.InvalidSuper;
//                 } else if (self.current_class != .Subclass) {
//                     return ResolveError.InvalidSuper;
//                 }
//                 try self.resolveLocal(expr, "super");
//             },
//             .This => |this| {
//                 if (self.current_class == .None) {
//                     return ResolveError.InvalidThis;
//                 }
//                 try self.resolveLocal(expr, "this");
//             },
//             .Unary => |unary| {
//                 try self.resolveExpr(unary.right);
//             },
//         }
//     };
//
//     pub fn resolveExpr(allocator: *Allocator, expr: Expr) !StringHashMap(usize) {
//         var resolver = Resolver.init(allocator);
//         defer resolver.deinit();
//
//         try resolver.resolveExpr(expr);
//         return resolver.locals;
//     }

//
//
// TESTS
//
//

// //const std = @import("std");
// const testing = std.testing;
// const Allocator = std.mem.Allocator;
//
// const Resolver = @import("resolver.zig");
// const Expr = @import("expr.zig").Expr;
// const Stmt = @import("stmt.zig").Stmt;
// const Token = @import("token.zig").Token;
// const Interpreter = @import("interpreter.zig");
// const Value = @import("value.zig").Value;
// const main = @import("main.zig");
//
// fn TestContext(comptime T: type) type {
//     return struct {
//         const Self = @This();
//
//         allocator: Allocator,
//         arena: std.heap.ArenaAllocator,
//         interpreter: Interpreter,
//         resolver: Resolver,
//         statements: std.ArrayList(Stmt),
//
//         fn init() !Self {
//             var arena = std.heap.ArenaAllocator.init(testing.allocator);
//             const allocator = arena.allocator();
//             var interpreter = try Interpreter.init(allocator);
//             var resolver = Resolver.init(allocator, &interpreter);
//
//             return Self{
//                 .allocator = allocator,
//                 .arena = arena,
//                 .interpreter = interpreter,
//                 .resolver = resolver,
//                 .statements = std.ArrayList(Stmt).init(allocator),
//             };
//         }
//
//         fn deinit(self: *Self) void {
//             self.statements.deinit();
//             self.arena.deinit();
//         }
//
//         fn addStatement(self: *Self, stmt: Stmt) !void {
//             try self.statements.append(stmt);
//         }
//
//         fn resolve(self: *Self) !void {
//             try self.resolver.resolveStatements(try self.statements.toOwnedSlice());
//         }
//     };
// }
//
// test "Resolver - Variable Declaration and Usage" {
//     var ctx = try TestContext(Resolver).init();
//     defer ctx.deinit();
//     // Test variable declaration
//     try ctx.addStatement(.{
//         .var_stmt = .{
//             .name = Token.make("x", 1, .{ .str = "x" }, .identifier),
//             .initializer = null,
//         },
//     });
//     // Test variable usage
//     const var_expr = &Expr{ .variable = Token.make("x", 2, .{ .str = "x" }, .identifier) };
//     try ctx.addStatement(.{ .expr_stmt = var_expr });
//     try ctx.resolve();
//     // Assert that the variable was properly resolved
//     try testing.expect(ctx.interpreter.locals.contains(var_expr));
// }
//
// test "Resolver - Scope Handling" {
//     var ctx = try TestContext(Resolver).init();
//     defer ctx.deinit();
//     try ctx.resolver.beginScope();
//     // Declare a variable in the inner scope
//     try ctx.addStatement(.{
//         .var_stmt = .{
//             .name = Token.make("y", 1, .{ .str = "y" }, .identifier),
//             .initializer = null,
//         },
//     });
//     ctx.resolver.endScope();
//     // Try to use the variable after its scope has ended
//     const var_expr = &Expr{ .variable = Token.make("y", 2, .{ .str = "y" }, .identifier) };
//     try ctx.addStatement(.{ .expr_stmt = var_expr });
//     try testing.expectError(Resolver.Error.undefined_variable, ctx.resolve());
// }
//
// test "Resolver - Function Declaration and Return" {
//     var ctx = try TestContext(Resolver).init();
//     defer ctx.deinit();
//     // Declare a function
//     try ctx.addStatement(.{
//         .function = .{
//             .name = Token.make("testFunc", 1, .{ .str = "testFunc" }, .identifier),
//             .parameters = &[_]Token{},
//             .body = &[_]Stmt{
//                 .{ .return_stmt = .{
//                     .value = @constCast(&Expr{ .literal = Value.Nil }),
//                     .keyword = Token.make("return", 2, .{ .str = "return" }, .identifier),
//                 } },
//             },
//         },
//     });
//     try ctx.resolve();
//     // No error should occur for a valid return inside a function
//     try testing.expect(!main.g_had_error);
// }
//
// test "Resolver - Invalid Top-Level Return" {
//     var ctx = try TestContext(Resolver).init();
//     defer ctx.deinit();
//     // Add a top-level return statement (which is invalid)
//     try ctx.addStatement(.{
//         .return_stmt = .{
//             .value = @constCast(&Expr{ .literal = Value.Nil }),
//             .keyword = Token.make("return", 1, .{ .str = "return" }, .identifier),
//         },
//     });
//     try ctx.resolve();
//     // Check error flags
//     try testing.expect(main.g_had_error);
//     try testing.expect(!main.g_had_runtime_error);
//     try testing.expect(main.g_error_count == 1);
//     // Check runtime values
//     try testing.expectEqualStrings("BANG  null", try std.fmt.allocPrint(ctx.allocator, "{any}", .{Interpreter.runtime_token}));
//     try testing.expectEqualStrings("false", try std.fmt.allocPrint(ctx.allocator, "{any}", .{Interpreter.runtime_return_value}));
//     try testing.expectEqualStrings("error.", try std.fmt.allocPrint(ctx.allocator, "{any}", .{Interpreter.runtime_error}));
// }
