//! Implements `LoxFunction` with entrypoint `functionCtxCallable(...)`.

const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;

const ErrorCode = @import("main.zig").ErrorCode;
const Expr = @import("expr.zig").Expr;
const Value = Expr.Value;
const LoxFunction = Value.LoxFunction;
const Interpreter = @import("interpreter.zig");
const Environment = @import("environment.zig");
const handleRuntimeError = Interpreter.handleRuntimeError;
const runtimeError = @import("main.zig").runtimeError;
const root = @import("root.zig");
const Stmt = @import("stmt.zig").Stmt;

pub const FunctionContext = struct {
    allocator: Allocator,
    /// This is the environment that is active when the function is declared
    /// not when it’s called, which is what we want. It represents the lexical
    /// scope surrounding the function declaration. Finally, when we call the
    /// function, we use that environment as the call’s parent instead of going
    /// straight to globals.
    closure: *Environment,
    declaration: Stmt.Function,

    pub fn handleRuntimeError(self: *FunctionContext, err: Interpreter.Error) void {
        root.eprint("Error in function context: '{s}': ", .{self.declaration.name.lexeme});
        Interpreter.handleRuntimeError(err) catch |e| {
            root.exit(@intFromEnum(ErrorCode.runtime_error), "{any}.", .{e});
        };
    }
};

fn arityFn(context: *anyopaque) usize {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    return ctx.declaration.parameters.len;
}

// This creates an environment chain that goes from the function’s body out
// through the environments where the function is declared, all the way out
// to the global scope. The runtime environment chain matches the textual
// nesting of the source code like we want. The end result when we call
// that function looks like this:
// var environment = Environment.initEnclosing(ctx.allocator, ctx.closure);
// var environment = ctx.allocator.create(Environment) catch |err| {
//     ctx.handleRuntimeError(err);
//     root.exit(1, "{}", .{err});
// };
// environment.* = ctx.closure.*;
// root.tracesrc(@src(), "====closure==== '{s}', '{any}'", .{
//     ctx.declaration.name.lexeme,
//     (environment.enclosing),
// });

// const upvalues = try vm.allocator.alloc(?*Upvalue, function.upvalueCount);
// // Need to null this out rather than leaving it
// // uninitialized becaue the GC might try to look at it
// // before it gets filled in with values
// for (upvalues) |*upvalue| upvalue.* = null;

// const previous = ctx.closure;
// defer {
//     ctx.closure = previous;
//     _ = ctx.closure.updateScopeDepth(.child_to_parent); // child -> parent
// }
// var environment = Environment.init(ctx.allocator) catch |err| { // Environment environment = new Environment(interpreter.globals);
//     ctx.handleRuntimeError(err);
//     return __temp_err_try;
// };
// environment = interpreter.globals;

// var environment = Environment.initEnclosing(ctx.allocator, previous);
// ctx.closure = &environment;
// _ = ctx.closure.updateScopeDepth(.parent_to_child); // parent -> child

fn callFn(context: *anyopaque, interpreter: *Interpreter, arguments: []Value) Value {
    const __temp_err_try = Value.Nil;
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));

    // const prev_environment = ctx.closure;
    // defer ctx.closure = prev_environment;
    // This is the environment that is active when the function is declared not
    // when it’s called, which is what we want. It represents the lexical scope
    // surrounding the function declaration. Finally, when we call the
    // function, we use that environment as the call’s parent instead of going
    // straight to globals.
    // var environment = Environment.initEnclosing(ctx.allocator, ctx.closure);
    // var environment = Environment.initEnclosing(ctx.allocator, prev_environment);
    // environment = interpreter.globals.*;
    // ctx.closure = &environment;
    var environment = Environment.init(ctx.allocator) catch |err| {
        ctx.handleRuntimeError(err);
        root.exit(70, "{any}: {any}", .{ ErrorCode.runtime_error, err });
    };
    environment = ctx.closure;

    for (ctx.declaration.parameters, 0..) |param, i| {
        ctx.closure.define(param.lexeme, arguments[i]) catch |err| {
            ctx.handleRuntimeError(err);
            return __temp_err_try;
        };
    }

    const __failing_test_enabled = false;
    if (comptime __failing_test_enabled)
        assert((if (Interpreter.runtime_error != undefined) Interpreter.runtime_token.type == .@"return" else Interpreter.runtime_error == undefined));

    // const result: Value = interpreter.execute(
    //     @constCast(&Stmt{ .block = ctx.declaration.body }),
    //     root.stdout().writer(),
    // ) catch |err|
    //     switch (err) {
    //     error.Return => blk: {
    //         assert(Interpreter.runtime_error == error.Return and Interpreter.runtime_token.type == .@"return");
    //         defer { // Reset thread-local variables runtime state captured on error
    //             Interpreter.runtime_error = undefined;
    //             Interpreter.runtime_return_value = undefined;
    //             Interpreter.runtime_token = undefined;
    //         }
    //         break :blk Interpreter.runtime_return_value;
    //     },
    //     else => blk: {
    //         ctx.handleRuntimeError(err);
    //         break :blk Value.Nil;
    //     },
    // };
    var out: Value = Value.Nil;
    _ = interpreter.executeBlock(ctx.declaration.body, environment, root.stdout().writer()) catch |err|
        switch (err) {
        error.Return => {
            assert(Interpreter.runtime_error == error.Return and Interpreter.runtime_token.type == .@"return");
            defer { // Reset thread-local variables runtime state captured on error
                Interpreter.runtime_error = undefined;
                Interpreter.runtime_return_value = undefined;
                Interpreter.runtime_token = undefined;
            }
            out = Interpreter.runtime_return_value;
        },
        else => {
            ctx.handleRuntimeError(err);
        },
    };

    return out;
}

fn toStringFn(context: *anyopaque) []const u8 {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    const buf = std.fmt.allocPrint(ctx.allocator, "<fn {s}>", .{ctx.declaration.name.lexeme}) catch |err| { // See also https://stackoverflow.com/a/66665672
        root.exit(70, "{any}: toStringCtxFn: {any}\n", .{ ErrorCode.runtime_error, err });
    };
    errdefer ctx.allocator.free(buf);

    return buf;
}

pub fn makeLoxFunction(
    allocator: Allocator,
    declaration: Stmt.Function,
    closure: *Environment,
) Allocator.Error!*LoxFunction {
    const context = try allocator.create(FunctionContext);
    errdefer allocator.destroy(context);
    context.* = .{ // aka `this` | `self`
        .allocator = allocator,
        .closure = closure,
        .declaration = declaration,
    };

    // Maybe we need to init context of values....!!! when initEnclosing
    // !!!!!!!!!!!!!11
    root.tracesrc(@src(), "===context.closure=== {any}", .{context.closure.scope_depth});

    const out = try allocator.create(LoxFunction);
    errdefer allocator.destroy(out);
    out.* = LoxFunction{
        .context = context, //> `*Context` casted to `*anyopaque`

        .arityFn = arityFn,
        .callFn = callFn,
        .toStringFn = toStringFn,
    };

    return out;
}

// pub const LoxFunction = struct {
//     context: *anyopaque, // Context pointer to hold function-specific data. e.g. `FunctionContext`
//
//     arityFn: *const fn (*anyopaque) usize,
//     callFn: *const fn (*anyopaque, *Interpreter, []Value) Value,
//     toStringFn: *const fn (*anyopaque) []const u8,
//
//     pub fn arity(self: *LoxFunction) usize {
//         return self.arityFn(self.context);
//     }
//
//     pub fn call(self: *LoxFunction, interpreter: *Interpreter, arguments: []Value) Value {
//         return self.callFn(self.context, interpreter, arguments);
//     }
//
//     pub fn toString(self: *LoxFunction) []const u8 {
//         return self.toStringFn(self.context);
//     }
// };
//
