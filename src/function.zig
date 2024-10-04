const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const Callable = @import("callable.zig");
const Instance = @import("instance.zig");
const Environment = @import("environment.zig");
const Expr = @import("expr.zig").Expr;
const Interpreter = @import("interpreter.zig");
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const Value = @import("value.zig").Value;
const logger = @import("logger.zig");
const root = @import("root.zig");
const debug = @import("debug.zig");

const Function = @This();

/// Callable provides `VTable`.
callable: Callable,
closure: *Environment,
declaration: Stmt.Function,
is_initializer: bool,

comptime {
    assert(@sizeOf(@This()) == 128);
    assert(@alignOf(@This()) == 8);
}

pub const Error = Callable.Error;

pub const RuntimeReturnError = Interpreter.PropagationException.return_value;

const vtable = Callable.VTable{
    .toString = toString,
    .call = call,
    .arity = arity,
};

pub fn init(
    allocator: Allocator,
    declaration: Stmt.Function,
    closure: *Environment,
    is_initializer: bool,
) Allocator.Error!*Function {
    const self = try allocator.create(Function);
    self.* = .{
        .callable = .{ .allocator = allocator, .vtable = &vtable },
        .closure = closure,
        .declaration = declaration,
        .is_initializer = is_initializer,
    };

    return self;
}

/// `self` should be the return value of `create`, or otherwise
/// have the same address and alignment property.
pub fn destroy(self: *Function, allocator: Allocator) void {
    // self.callable.destroy(allocator);
    allocator.destroy(self);
}

/// TODO: Let Stmt.Function implement this.
pub fn is_init_method(declaration: *const Stmt.Function) bool {
    return mem.eql(u8, declaration.name.lexeme, "init");
}

// assert(mem.eql(u8, self.declaration.name.lexeme, "init") and mem.eql(u8, callable.toString(), "<fn init>"));
// const this_token: Token = blk: { // can just assign `lexeme` to "this", but seems hacky.
//     const stmt: *const Stmt = &.{ .function = self.declaration };
//     break :blk if (stmt.extractThisExpr()) |this| this.keyword else @panic("unreachable");
// };
//
// const this_value = self.closure.getAt(0, this_token) catch |err| switch (err) {
//     error.OutOfMemory => |alloc_err| {
//         Interpreter.runtime_token = this_token;
//         try Interpreter.handleRuntimeError(alloc_err);
//         return alloc_err;
//     },
//     error.variable_not_declared => |env_err| {
//         Interpreter.runtime_token = this_token;
//         Interpreter.panicRuntimeError(env_err, this_token);
//         unreachable;
//     },
// };
//
// if (comptime debug.is_trace_interpreter) logger.debug(.default, @src(),
//     \\Forcibly returning "this" value at closure of distance '0'.
//     \\{s}declaration: {}.{s}callable: {s}.{s}!this instance: {any}
// , .{ logger.indent, self.declaration.name, logger.newline, callable.toString(), logger.newline, this_value });
//
// return this_value;
fn returnThis(self: *Function) Value {
    const this_token = self.declaration.name;
    const this_value = try self.closure.getAt(0, this_token);
    return this_value;
}

/// Creates a new environment within the method’s closure, binding "this" to
/// the instance. This ensures the method retains the bound instance for future
/// calls. Interpreting "this" expressions works like variable expressions.
///
/// See https://craftinginterpreters.com/classes.html#this
/// See `Interpreter.visitThisExpr` in interpreter.zig.
pub fn bind(self: *Function, instance: *Instance) Allocator.Error!*Function {
    var allocator: Allocator = self.callable.allocator;

    const is_initializer = Function.is_init_method(&self.declaration);

    const previous_env: *Environment = self.closure;
    defer self.closure = previous_env;

    var environment: *Environment = try Environment.init(allocator);
    errdefer allocator.destroy(environment);
    environment = self.closure;

    { // We guarantee that closures always have an enclosing environment.
        defer self.closure.enclosing = environment;

        const object: *Value = try allocator.create(Value);
        errdefer allocator.destroy(object);
        object.* = .{ .instance = instance };

        environment.define("this", object.*) catch |err| {
            Interpreter.runtime_token = self.declaration.name;
            try Interpreter.handleRuntimeError(err);
        };
    }

    return try Function.init(allocator, self.declaration, environment, is_initializer);
}

pub fn toString(callable: *const Callable) []const u8 {
    const self: *Function = @constCast(@fieldParentPtr("callable", callable));

    const token: Token = self.declaration.name;
    const buffer = std.fmt.allocPrint(callable.allocator, "<fn {s}>", .{
        token.lexeme,
    }) catch |err| {
        Interpreter.panicRuntimeError(err, token);
    };

    return buffer;
}

/// Caller may set `runtime_token` while handling `Allocator.Error`.
pub fn call(
    callable: *const Callable,
    interpreter: *Interpreter,
    arguments: []Value,
) Function.Error!Value {
    const self: *Function = @constCast(@fieldParentPtr("callable", callable));

    var environment = Environment.init(callable.allocator) catch |err| {
        Interpreter.runtime_token = self.declaration.name;
        try Interpreter.handleRuntimeError(err);
        return err; // bail early with error.
    };
    errdefer callable.allocator.destroy(environment);
    environment = self.closure;

    for (self.declaration.parameters, 0..) |param, i| {
        // TODO: use handle error.... this is wip as we build upon class,
        // methods, inheritance, etc...
        if (i >= arguments.len) {
            @panic("Parameter and argument count mismatch");
        }
        self.closure.define(param.lexeme, arguments[i]) catch |err| {
            Interpreter.runtime_token = param;
            try Interpreter.handleRuntimeError(err);
            continue; // allow other parameters to proceed and stack up all runtime errors
        };
    }

    interpreter.executeBlock(
        self.declaration.body,
        .{ .existing = environment },
        root.stdout().writer(),
    ) catch |err| switch (err) {
        RuntimeReturnError => {
            assert((Interpreter.runtime_error == RuntimeReturnError) and
                (Interpreter.runtime_token.type == .@"return"));
            defer {
                Interpreter.runtime_error = undefined;
                Interpreter.runtime_return_value = undefined;
                Interpreter.runtime_token = undefined;
            }
            return Interpreter.runtime_return_value;
        },
        else => {
            try Interpreter.handleRuntimeError(err);
            return Value.Nil;
        },
    };

    if (self.is_initializer) {
        root.assume(mem.eql(u8, self.declaration.name.lexeme, "init") and
            mem.eql(u8, callable.toString(), "<fn init>"), .allow);

        return self.closure.getAtAuto([]const u8, 0, "this") catch |err| blk: {
            const stmt: *const Stmt = &.{ .function = self.declaration };
            Interpreter.runtime_token = if (stmt.extractThisExpr()) |this| this.keyword else unreachable;
            switch (err) {
                error.OutOfMemory => try Interpreter.handleRuntimeError(err),
                inline else => Interpreter.panicRuntimeError(err, Interpreter.runtime_token),
            }

            break :blk Error.OutOfMemory;
        };
    }

    return Value.Nil;
}

pub fn arity(callable: *const Callable) usize {
    const self: *Function = @constCast(@fieldParentPtr("callable", callable));

    return self.declaration.parameters.len;
}

test "stats" {
    try testing.expectEqual(120, @sizeOf(@This()));
    try testing.expectEqual(8, @alignOf(@This()));
}

test "Function initialization" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const env = try Environment.init(allocator);
    const declaration = Stmt.Function{
        .name = Token{
            .type = .identifier,
            .lexeme = "test_function",
            .line = 1,
            .literal = null,
        },
        .parameters = &[_]Token{},
        .body = &[_]Stmt{},
    };

    const func = try Function.init(allocator, env, declaration);

    try testing.expect(func.closure == env);
    try testing.expectEqualStrings("test_function", func.declaration.name.lexeme);
}

test "Function toString" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const env = try Environment.init(allocator);
    const declaration = Stmt.Function{
        .name = Token{
            .type = .identifier,
            .lexeme = "test_functon",
            .line = 1,
            .literal = null,
        },
        .parameters = &[_]Token{},
        .body = &[_]Stmt{},
    };

    const func = try Function.init(allocator, env, declaration);

    const result = func.callable.toString();

    try testing.expectEqualStrings("<fn test_functon>", result);
}

test "Function arity" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const env = try Environment.init(allocator);

    const function_declaration = Stmt.Function{
        .name = Token{
            .type = .identifier,
            .lexeme = "test_function",
            .line = 1,
            .literal = null,
        },
        .parameters = @constCast(&[_]Token{
            Token{
                .type = .identifier,
                .lexeme = "a",
                .line = 1,
                .literal = null,
            },
            Token{
                .type = .identifier,
                .lexeme = "b",
                .line = 1,
                .literal = null,
            },
        }),
        .body = &[_]Stmt{},
    };

    const func = try Function.init(allocator, env, function_declaration);

    try testing.expectEqual(@as(usize, 2), func.callable.arity());
}
