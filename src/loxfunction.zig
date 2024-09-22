const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const Environment = @import("environment.zig").Environment;
const ErrorCode = @import("main.zig").ErrorCode;
const Expr = @import("expr.zig").Expr;
const Interpreter = @import("interpreter.zig");
const Value = Expr.Value;
const root = @import("root.zig");
const exit = root.exit;
const stdout = root.stdout;
const Stmt = @import("stmt.zig").Stmt;

pub const FunctionContext = struct {
    allocator: Allocator,
    declaration: Stmt.Function,
};

fn arityCtxFn(context: *anyopaque) usize {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    return ctx.declaration.parameters.len;
}

fn callCtxFn(context: *anyopaque, interpreter: *Interpreter, arguments: []Value) Value {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    const len = ctx.declaration.parameters.len;
    var env = Environment.initEnclosing(interpreter.environment, ctx.allocator);
    errdefer env.deinit();

    var i: usize = 0;
    while (i < len) : (i += 1) {
        env.define(ctx.declaration.parameters[i].lexeme, arguments[i]) catch |err| {
            exit(70, "{any}: callCtxFn: {any}\n", .{ ErrorCode.runtime_error, err });
        };
    }

    var stmt: Stmt = .{ .block = ctx.declaration.body };

    return interpreter.execute(&stmt, stdout().writer()) catch |err|
        exit(70, "{any}: callCtxFn: {any}\n", .{ ErrorCode.runtime_error, err });
}

/// Can manually deallocate. (note: without free found no leaks in valgrind)
/// See also https://stackoverflow.com/a/66665672
fn toStringCtxFn(context: *anyopaque) []const u8 {
    const ctx = @as(*FunctionContext, @alignCast(@ptrCast(context)));
    const name: []const u8 = ctx.declaration.name.lexeme;
    const buf = std.fmt.allocPrint(ctx.allocator, "<fn {s}>", .{name}) catch |err| {
        exit(70, "{any}: toStringCtxFn: {any}\n", .{ ErrorCode.runtime_error, err });
    };
    errdefer ctx.allocator.free(buf);

    return buf;
}

pub fn functionCtxCallable(allocator: Allocator, declaration: Stmt.Function) Allocator.Error!*Value.LoxFunction {
    const context = try allocator.create(FunctionContext);
    errdefer allocator.destroy(context);
    context.* = .{
        .allocator = allocator,
        .declaration = declaration,
    };

    const out = try allocator.create(Value.LoxFunction);
    errdefer allocator.destroy(out);
    out.* = Value.LoxFunction{
        .arityFn = arityCtxFn,
        .callFn = callCtxFn,
        .toStringFn = toStringCtxFn,
        .context = context, //> `*Context` cast to `*anyopaque`
    };

    return out;
}

// fn arityFn(context: *anyopaque) usize {
//     const declaration = @as(*Stmt.Function, @alignCast(@ptrCast(context)));
//
//     return declaration.parameters.len;
// }
//
// fn callFn(context: *anyopaque, interpreter: *Interpreter, arguments: []Value) Value {
//     const declaration = @as(*Stmt.Function, @alignCast(@ptrCast(context)));
//     var environment = Environment.initEnclosing(interpreter.environment, interpreter.allocator);
//
//     const len = declaration.parameters.len;
//     var i: usize = 0;
//     while (i < len) : (i += 1) {
//         environment.define(declaration.parameters[i].lexeme, arguments[i]) catch unreachable;
//     }
//
//     // HACK: should use actual writer
//     const stdout_file = std.io.getStdOut();
//     var stmt: Stmt = .{ .block = declaration.body };
//     // Return?
//     // return Value.Nil;
//     return interpreter.execute(&stmt, stdout_file.writer()) catch |err| {
//         std.log.err("callFn: {any}", .{err});
//         unreachable;
//     };
// }
//
// /// TODO: use context.allocator
// /// Can manually deallocate. (note: without free found no leaks in valgrind)
// /// See also https://stackoverflow.com/a/66665672
// fn toStringFn(context: *anyopaque) []const u8 {
//     const declaration = @as(*Stmt.Function, @alignCast(@ptrCast(context)));
//
//     // NOTE: defer free leads to accessing unitialized value error
//     const page_allocator = std.heap.page_allocator; // makes a syscall directly for every allocation and free
//     const buf: []u8 = std.fmt.allocPrint(page_allocator, "<fn {s}>", .{declaration.name.lexeme}) catch |err| {
//         // TODO: Use sensible error handling
//         std.log.err("toStringFn: {any}", .{err});
//         return "<fn !error>";
//     };
//     errdefer page_allocator.free(buf);
//
//     return buf;
// }
//
// pub fn functionCallable(allocator: Allocator, declaration: Stmt.Function) Allocator.Error!*LoxFunction {
//     const fun = try allocator.create(Stmt.Function);
//     errdefer allocator.destroy(fun);
//     fun.* = declaration;
//
//     const out = try allocator.create(Value.LoxFunction);
//     errdefer allocator.destroy(out);
//     out.* = Value.LoxFunction{
//         .arityFn = arityFn,
//         .callFn = callFn,
//         .toStringFn = toStringFn,
//         .context = fun, //> `*Function` cast to `*anyopaque`
//     };
//
//     return out;
// }
