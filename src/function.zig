const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const Callable = @import("callable.zig");
const Environment = @import("environment.zig");
const Expr = @import("expr.zig").Expr;
const Interpreter = @import("interpreter.zig");
const Stmt = @import("stmt.zig").Stmt;
const Token = @import("token.zig");
const Value = @import("value.zig").Value;
const logger = @import("logger.zig");
const root = @import("root.zig");

const Function = @This();

/// Callable provides `VTable`.
callable: Callable,
closure: *Environment,
declaration: Stmt.Function,

comptime {
    assert(@sizeOf(@This()) == 120);
    assert(@alignOf(@This()) == 8);
}

pub const Error = Callable.Error;

pub fn init(
    allocator: Allocator,
    closure: *Environment,
    declaration: Stmt.Function,
) Allocator.Error!*Function {
    const self = try allocator.create(Function);
    self.* = .{
        .callable = .{
            .allocator = allocator,
            .vtable = &vtable,
        },
        .closure = closure,
        .declaration = declaration,
    };

    return self;
}

/// `self` should be the return value of `create`, or otherwise
/// have the same address and alignment property.
pub fn destroy(self: *Function, allocator: Allocator) void {
    // self.callable.destroy(allocator);
    allocator.destroy(self);
}

const vtable = Callable.VTable{
    .toString = toString,
    .call = call,
    .arity = arity,
};

pub fn toString(callable: *const Callable) []const u8 {
    const self: *Function = @constCast(@fieldParentPtr("callable", callable));

    const token: Token = self.declaration.name;
    const buffer = std.fmt.allocPrint(callable.allocator, "<fn {s}>", .{token.lexeme}) catch |err| {
        Interpreter.panicRuntimeError(err, token);
    };

    return buffer;
}

pub fn call(callable: *const Callable, interpreter: *Interpreter, arguments: []Value) Function.Error!Value {
    const self: *Function = @constCast(@fieldParentPtr("callable", callable));

    var environment = Environment.init(callable.allocator) catch |err| {
        // TODO: Caller should pre-set runtime_token for error
        // try Interpreter.handleRuntimeError(err);
        return err;
    };
    errdefer callable.allocator.destroy(environment);

    environment = self.closure;

    for (self.declaration.parameters, 0..) |param, i| {
        self.closure.define(param.lexeme, arguments[i]) catch |err| {
            // TODO: Should the `call()` caller pre-set runtime_token for error??
            Interpreter.runtime_token = param;
            try Interpreter.handleRuntimeError(err);
            // NOTE: Cannot return Environment specific errors, unless `Callable.Error` includes them
            //   return err;
        };
    }

    const body: Stmt.Block = self.declaration.body;
    const closure: Environment.Closure = .{ .existing = environment };
    const writer = root.stdout().writer();

    const result = interpreter.executeBlock(body, closure, writer);

    result catch |err| switch (err) {
        error.@"return" => {
            assert((Interpreter.runtime_error == error.@"return") and
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
