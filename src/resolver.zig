//! Resolves each variable definition and usage with it's corresponding scope.
//! Stores the scope number in the `locals` which is returned as an artifacto
//! of calling the resolver.

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;

const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

const Expr = @import("expr.zig").Expr;
const Interpreter = @import("interpreter.zig");
const main = @import("main.zig");
const root = @import("root.zig");
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");

/// Static analysis before running interpretter.
const Resolver = @This(); // Struct File

allocator: Allocator,
interpreter: *Interpreter, // provides locals: AutoHashMap(*Expr, usize),
scopes: ArrayList(BlockScope),
current_function: FunctionType = FunctionType.none,

/// Keys, as in Environment, are variable names.
/// The values are Booleans, for a reason I’ll explain soon.
const BlockScope = StringHashMap(bool);

const FunctionType = enum {
    none,
    function,
    initializer,
    method,
};

const ClassType = enum {
    none,
    class,
    subclass,
};

pub fn init(allocator: Allocator, interpreter: *Interpreter) Resolver {
    const self: Resolver = .{
        .allocator = allocator,
        .interpreter = interpreter,
        .scopes = ArrayList(StringHashMap(bool)).init(allocator),
    };

    return self;
}

pub fn deinit(self: *Resolver) void {
    _ = self;
    std.log.warn("Tried to deinit unimplemented Resolver.deinit()", .{});
}

fn isEmptyScopes(self: *Resolver) bool {
    return (self.scopes.items.len == 0);
}

fn getLastOrNullScopesPtr(self: *Resolver) ?*BlockScope {
    return if (!self.isEmptyScopes())
        &self.scopes.items[self.scopes.items.len - 1]
    else
        null;
}

fn beginScope(self: *Resolver) Error!void {
    // `std.ArrayList` provides the methods necessary to use it as a stack.
    // Lexical scopes nest in both the interpreter and the resolver. They behave
    // like a stack. The interpreter implements that stack using a linked list—the
    // chain of Environment objects.
    try self.scopes.append(StringHashMap(bool).init(self.allocator));
}

pub const ResolveError = error{ // InvalidSuper, InvalidThis, SelfInheritance,
    AlreadyDeclared,
    InvalidReturn,
    UndefinedVariable,
};

pub const Error = ResolveError || Allocator.Error;

pub fn handleResolveError(err: Error, token: Token, comptime fmt: []const u8, args: anytype) Allocator.Error!void {
    return switch (err) {
        error.AlreadyDeclared => main.runtimeError(token, fmt, args),
        error.InvalidReturn => main.runtimeError(token, fmt, args),
        error.UndefinedVariable => main.runtimeError(token, fmt, args),
        else => |other| other,
    };
}

test "stack" {
    const test_allocator = testing.allocator;
    const expect = testing.expect;

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
        try expect(std.meta.eql(pair, switch (i) {
            0 => Pair{ .open = 1, .close = 2 },
            1 => Pair{ .open = 3, .close = 4 },
            2 => Pair{ .open = 0, .close = 5 },
            else => unreachable,
        }));
    }
}

fn endScope(self: *Resolver) void {
    // The scope stack is only used for local block scopes. Variables declared at
    // the top level in the global scope are not tracked by the resolver since they
    // are more dynamic in Lox. When resolving a variable, if we can’t find it in
    // the stack of local scopes, we assume it must be global.

    var scope: BlockScope = self.scopes.pop();

    var it = scope.iterator();
    while (it.next()) |entry| {
        const name: []const u8 = entry.key_ptr.*;
        const is_used: bool = entry.value_ptr.*;
        const is_not_used = (!is_used and !mem.startsWith(u8, name, "_") and
            !mem.eql(u8, name, "this") and !mem.eql(u8, name, "super"));
        if (is_not_used) {
            root.eprint(("Warning: Unused local variable '{s}'. " ++
                "Start variable name with underscore if variable is unused.\n"), .{name});
        }
    }

    scope.deinit();
}

// Declaration adds the variable to the innermost scope so that it shadows any
// outer one and so that we know the variable exists. We mark it as “not ready
// yet” by binding its name to false in the scope map. The value associated
// with a key in the scope map represents whether or not we have finished
// resolving that variable’s initializer.
fn declare(self: *Resolver, name: Token) Error!void {
    if (self.getLastOrNullScopesPtr()) |scope_ptr| {
        if (scope_ptr.contains(name.lexeme)) {
            return handleResolveError(
                ResolveError.AlreadyDeclared,
                name,
                "Already a variable with this name in this scope.",
                .{},
            );
        } else {
            try scope_ptr.put(name.lexeme, false);
        }
    } else return;
}

// After declaring the variable, we resolve its initializer expression in that
// same scope where the new variable now exists but is unavailable. Once the
// initializer expression is done, the variable is ready for prime time. We do
// that by defining it.
fn define(self: *Resolver, name: Token) Error!void {
    if (self.getLastOrNullScopesPtr()) |scope_ptr| {
        try scope_ptr.put(name.lexeme, true);
    } else return;
}

// private void resolveLocal(Expr expr, Token name) {
//   for (int i = scopes.size() - 1; i >= 0; i--) {
//     if (scopes.get(i).containsKey(name.lexeme)) {
//       interpreter.resolve(expr, scopes.size() - 1 - i);
//       return;
//     }
//   }
// }
fn resolveLocal(self: *Resolver, expr: *const Expr, name: Token) Error!void {
    // This looks, for good reason, a lot like the code in Environment for
    // evaluating a variable. We start at the innermost scope and work outwards,
    // looking in each map for a matching name. If we find the variable, we resolve
    // it, passing in the number of scopes between the current innermost scope and
    // the scope where the variable was found. So, if the variable was found in the
    // current scope, we pass in 0. If it’s in the immediately enclosing scope, 1.
    const key = name.lexeme;
    const stack_index: usize = self.scopes.items.len - 1;
    var i: usize = stack_index;

    while (i >= 0) : (i -= 1) {
        const found_variable = self.scopes.items[i].contains(key);
        if (found_variable) {
            const scope_delta: usize = stack_index - i;
            try self.interpreter.locals.put(@constCast(expr), scope_delta);
            return;
        }
    }
    // If we walk through all of the block scopes and never find the variable,
    // we leave it unresolved and assume it’s global. We’ll get to the
    // implementation of that resolve() method a little later. For now, let’s
    // keep on cranking through the other syntax nodes.
}

fn resolveFunction(self: *Resolver, function: *const Stmt.Function, @"type": FunctionType) Error!void {
    // We need to track not just that we’re in a function, but how many we’re in.

    const enclosing_function = self.current_function;

    // We could use an explicit stack of FunctionType values for that, but
    // instead we’ll piggyback on the JVM. We store the previous value in a
    // local on the Java stack. When we’re done resolving the function body, we
    // restore the field to that value.

    self.current_function = @"type";
    defer self.current_function = enclosing_function;

    try self.beginScope();
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
            if (root.unionPayloadPtr(Token, stmt)) |token_ptr| {
                root.tracesrc(@src(), "Found payload ptr: {any}: {any}", .{ stmt, token_ptr });
                return handleResolveError(err, token_ptr.*, "Failed to resolve statement '{any}'.", .{stmt});
            }
        };
    }
}

/// Since the resolver needs to visit every node in the syntax tree, it
/// implements the visitor abstraction we already have in place. Only a few
/// kinds of nodes are interesting when it comes to resolving variables:
///
/// * A block statement introduces a new scope for the statements it contains.
/// * A function declaration introduces a new scope for its body and binds its
///   parameters in that scope.
/// * A variable declaration adds a new variable to the current scope.
/// * Variable and assignment expressions need to have their variables resolved.
///
/// The rest of the nodes don’t do anything special, but we still need to
/// implement visit methods for them that traverse into their subtrees. Even
/// though a + expression doesn’t itself have any variables to resolve, either
/// of its operands might.
///
/// See also:
/// * https://craftinginterpreters.com/resolving-and-binding.html#a-resolver-class
/// * https://gitlab.com/andreyorst/lox/-/blob/main/src/clojure/lox/resolver.clj?ref_type=heads
pub fn resolveStatement(self: *Resolver, stmt: *const Stmt) Error!void {
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
            if (return_stmt.value) |expr| {
                try self.resolveExpr(expr);
            }
        },
        // Resolving a variable declaration adds a new entry to the current
        // innermost scope’s map.
        .var_stmt => |var_stmt| {
            try self.declare(var_stmt.name);
            if (var_stmt.initializer) |expr| {
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

// These methods are similar to the evaluate() and execute() methods in
// Interpreter—they turn around and apply the Visitor pattern to the given
// syntax tree node.
pub fn resolveExpr(self: *Resolver, expr: *Expr) Error!void {
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
            // Static analysis does no control flow or short-circuiting
            try self.resolveExpr(logical.left);
            try self.resolveExpr(logical.right);
        },
        .unary => |unary| {
            try self.resolveExpr(unary.right);
        },
        .variable => |variable| {
            const name = variable.lexeme;
            if (self.getLastOrNullScopesPtr()) |scope_ptr| {
                if (scope_ptr.get(name)) |is_declared| {
                    if (!is_declared) {
                        return handleResolveError(
                            ResolveError.UndefinedVariable,
                            variable,
                            "Can't read local variable '{s}' in its own initializer.",
                            .{name},
                        );
                    }
                }
            }
            try self.resolveLocal(expr, variable);
        },
    }
}

pub fn resolveExpression(allocator: Allocator, interpreter: *Interpreter, expr: *Expr) Error!std.AutoHashMap(*Expr, usize) {
    const resolver: Resolver = try Resolver.init(allocator, interpreter);
    defer resolver.deinit();
    try resolver.resolveExpr(expr);

    return resolver.interpreter.locals;
}

// const ResolveError = error{
//     AlreadyDeclared,
//     UndefinedVariable,
//     InvalidThis,
//     InvalidSuper,
//     InvalidReturn,
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
//             return ResolveError.AlreadyDeclared;
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
//                     return ResolveError.InvalidReturn;
//                 }
//                 if (return_stmt.value) |value| {
//                     if (self.current_function == .Initializer) {
//                         return ResolveError.InvalidReturn;
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
//                             return ResolveError.UndefinedVariable;
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
