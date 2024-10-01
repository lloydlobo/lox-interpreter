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

callable: Callable, // provides VTable
closure: *Environment,
declaration: Stmt.Function,

comptime {
    // assert(@sizeOf(@This()) == 40);
    assert(@alignOf(@This()) == 8);
}

pub fn init(allocator: Allocator, closure: *Environment, declaration: Stmt.Function) Allocator.Error!*Function {
    const self = try allocator.create(Function);
    self.* = .{
        .callable = .{ .allocator = allocator, .vtable = &vtable },
        .closure = closure,
        .declaration = declaration,
    };
    return self;
}

const vtable = Callable.VTable{
    .toString = toString,
    .call = call,
    .arity = arity,
};

pub const AllocPrintError = error{OutOfMemory};
fn toString(callable: *const Callable) AllocPrintError![]const u8 {
    const self: *Function = @constCast(@fieldParentPtr("callable", callable));
    const buffer: []u8 = try std.fmt.allocPrint(callable.allocator, "{s}", .{self.declaration.name.lexeme});
    return buffer;
}

fn call(callable: *const Callable, interpreter: *Interpreter, arguments: []Value) Value {
    const self: *Function = @constCast(@fieldParentPtr("callable", callable));

    var environment = Environment.init(interpreter.allocator) catch |err| {
        Interpreter.handleRuntimeError(err) catch unreachable;
        root.exit(.runtime_error, "Failed to initialize environment while calling '{any}': {any}", .{
            callable.toString(),
            err,
        });
    };
    errdefer interpreter.allocator.destroy(environment);
    environment = self.closure;

    for (self.declaration.parameters, 0..) |param, i| {
        self.closure.define(param.lexeme, arguments[i]) catch |err| {
            Interpreter.handleRuntimeError(err) catch unreachable;
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
            Interpreter.handleRuntimeError(err) catch unreachable;
            return Value.Nil;
        },
    };

    return Value.Nil;
}

fn arity(callable: *const Callable) usize {
    const self: *Function = @constCast(@fieldParentPtr("callable", callable));

    return self.declaration.parameters.len;
}

test "basic usage" {
    try testing.expectEqual(24, @sizeOf(@This()));
    try testing.expectEqual(8, @alignOf(@This()));
}
