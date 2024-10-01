const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const Expr = @import("expr.zig").Expr;
const Interpreter = @import("interpreter.zig");
const Token = @import("token.zig");
const Value = @import("value.zig").Value;
const formatNumber = @import("root.zig").formatNumber;
const logger = @import("logger.zig");

const Callable = @This();

allocator: Allocator,
vtable: *const VTable,

const builtin = @import("builtin/root.zig");
const core = @import("core/root.zig");

pub const default_vtable = builtin.default_vtable;
pub const clock_vtable = builtin.clock_vtable;

pub const AllocPrintError = error{OutOfMemory};
pub const Error = AllocPrintError;

pub const VTable = struct {
    toString: *const fn (*const Callable) AllocPrintError![]const u8,
    call: *const fn (*const Callable, *Interpreter, []Value) Value,
    arity: *const fn (*const Callable) usize,
};

/// Returns a pointer to undefined memory.
/// Call `destroy` with the result to free the memory.
pub fn init(allocator: Allocator) Allocator.Error!*Callable {
    const out = try allocator.create(Callable);
    errdefer allocator.destroy(out);
    out.* = .{
        .allocator = allocator,
        .vtable = undefined,
    };

    return out;
}

/// `self` should be the return value of `create`, or otherwise
/// have the same address and alignment property.
pub fn destroy(self: *Callable, allocator: Allocator) void {
    allocator.destroy(self);
}

pub fn toString(self: *const Callable) AllocPrintError![]const u8 {
    return try self.vtable.toString(self);
}

pub fn call(self: *const Callable, interpreter: *Interpreter, arguments: []Value) Value {
    return self.vtable.call(self, interpreter, arguments);
}

pub fn arity(self: *const Callable) usize {
    return self.vtable.arity(self);
}

// pub const default_vtable: *const Callable.VTable = &.{
//     .toString = struct {
//         fn toString(_: *const Callable) AllocPrintError![]const u8 {
//             return "<native fn>";
//         }
//     }.toString,
//     .call = struct {
//         pub fn call(_: *const Callable, _: *Interpreter, _: []Value) Value {
//             return Value.Nil;
//         }
//     }.call,
//     .arity = struct {
//         fn arity(_: *const Callable) usize {
//             return 0;
//         }
//     }.arity,
// };
//
// pub const clock_vtable: *const VTable = &.{
//     .toString = struct {
//         fn toString(self: *const Callable) AllocPrintError![]const u8 {
//             _ = self; // autofix
//             return "<native fn>";
//         }
//     }.toString,
//
//     .call = struct {
//         pub fn call(_: *const Callable, _: *Interpreter, _: []Value) Value {
//             return .{ .num = (@as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0) };
//         }
//     }.call,
//
//     .arity = struct {
//         fn arity(self: *const Callable) usize {
//             _ = self; // autofix
//             return 0;
//         }
//     }.arity,
// };

test "Callable ─ native clock function" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const clock: Callable = .{
        .allocator = allocator,
        .vtable = clock_vtable,
    };

    var interpreter = try Interpreter.init(allocator);
    const arguments = &[_]Value{};

    try testing.expectEqualStrings("<native fn>", try clock.toString());

    const actual: f64 = clock.call(&interpreter, arguments).num;
    const expected = (@as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0);
    try testing.expectEqual(expected, actual);

    try testing.expectEqual(0, clock.arity());
}

// pub const LoxCallable = struct { // native function
//     arityFn: *const fn () usize,
//     callFn: *const fn (*Interpreter, []Value) Value,
//     toStringFn: *const fn () []const u8,
//
//     comptime {
//         assert(@sizeOf(@This()) == 24);
//         assert(@alignOf(@This()) == 8);
//     }
//
//     pub fn arity(self: *const LoxCallable) usize {
//         return self.arityFn();
//     }
//     pub fn call(self: *const LoxCallable, interpreter: *Interpreter, arguments: []Value) Value {
//         return self.callFn(interpreter, arguments);
//     }
//     pub fn toString(self: *const LoxCallable) []const u8 {
//         return self.toStringFn();
//     }
// };

// const std = @import("std");
// const assert = std.debug.assert;
// const testing = std.testing;
//
// const Interpreter = @import("interpreter.zig");
// const Token = @import("token.zig");
// const formatNumber = @import("root.zig").formatNumber;
// const logger = @import("logger.zig");
//
// pub const Expr = union(enum) {
//     // ... (previous Expr definition remains unchanged)
//
//     pub const Value = union(enum) {
//         bool: bool,
//         nil,
//         num: f64,
//         str: []const u8,
//         ret: *LoxReturnValue,
//         callable: *const LoxCallable,
//
//         // ... (other Value methods remain unchanged)
//
//         pub const LoxCallable = struct {
//             vtable: *const VTable,
//
//             pub const VTable = struct {
//                 arity: *const fn (*const LoxCallable) usize,
//                 call: *const fn (*const LoxCallable, *Interpreter, []Value) Value,
//                 toString: *const fn (*const LoxCallable) []const u8,
//             };
//
//             pub fn arity(self: *const LoxCallable) usize {
//                 return self.vtable.arity(self);
//             }
//
//             pub fn call(self: *const LoxCallable, interpreter: *Interpreter, arguments: []Value) Value {
//                 return self.vtable.call(self, interpreter, arguments);
//             }
//
//             pub fn toString(self: *const LoxCallable) []const u8 {
//                 return self.vtable.toString(self);
//             }
//         };
//
//         pub const LoxFunction = struct {
//             callable: LoxCallable,
//             closure: *Environment,
//             declaration: *FunctionStmt,
//
//             pub fn init(closure: *Environment, declaration: *FunctionStmt) LoxFunction {
//                 return .{
//                     .callable = .{ .vtable = &vtable },
//                     .closure = closure,
//                     .declaration = declaration,
//                 };
//             }
//
//             const vtable = LoxCallable.VTable{
//                 .arity = arity,
//                 .call = call,
//                 .toString = toString,
//             };
//
//             fn arity(callable: *const LoxCallable) usize {
//                 const self = @fieldParentPtr(LoxFunction, "callable", callable);
//                 return self.declaration.params.len;
//             }
//
//             fn call(callable: *const LoxCallable, interpreter: *Interpreter, arguments: []Value) Value {
//                 const self = @fieldParentPtr(LoxFunction, "callable", callable);
//                 var environment = Environment.init(interpreter.allocator, self.closure);
//                 defer environment.deinit();
//
//                 for (self.declaration.params, 0..) |param, i| {
//                     environment.define(param.lexeme, arguments[i]);
//                 }
//
//                 const result = interpreter.executeBlock(self.declaration.body, &environment);
//                 return switch (result) {
//                     .ok => .nil,
//                     .err => |err| switch (err) {
//                         .return_value => |value| value,
//                         else => result,
//                     },
//                 };
//             }
//
//             fn toString(callable: *const LoxCallable) []const u8 {
//                 const self = @fieldParentPtr(LoxFunction, "callable", callable);
//                 return self.declaration.name.lexeme;
//             }
//         };
//
//         pub const LoxClass = struct {
//             callable: LoxCallable,
//             name: []const u8,
//             methods: std.StringHashMap(*LoxFunction),
//
//             pub fn init(name: []const u8, methods: std.StringHashMap(*LoxFunction)) LoxClass {
//                 return .{
//                     .callable = .{ .vtable = &vtable },
//                     .name = name,
//                     .methods = methods,
//                 };
//             }
//
//             const vtable = LoxCallable.VTable{
//                 .arity = arity,
//                 .call = call,
//                 .toString = toString,
//             };
//
//             fn arity(callable: *const LoxCallable) usize {
//                 const self = @fieldParentPtr(LoxClass, "callable", callable);
//                 if (self.methods.get("init")) |initializer| {
//                     return initializer.callable.arity();
//                 }
//                 return 0;
//             }
//
//             fn call(callable: *const LoxCallable, interpreter: *Interpreter, arguments: []Value) Value {
//                 const self = @fieldParentPtr(LoxClass, "callable", callable);
//                 const instance = LoxInstance.init(self);
//                 if (self.methods.get("init")) |initializer| {
//                     _ = initializer.callable.call(interpreter, arguments);
//                 }
//                 return .{ .instance = instance };
//             }
//
//             fn toString(callable: *const LoxCallable) []const u8 {
//                 const self = @fieldParentPtr(LoxClass, "callable", callable);
//                 return self.name;
//             }
//         };
//
//         pub const LoxInstance = struct {
//             class: *const LoxClass,
//             fields: std.StringHashMap(Value),
//
//             pub fn init(class: *const LoxClass) *LoxInstance {
//                 const instance = allocator.create(LoxInstance) catch @panic("Out of memory");
//                 instance.* = .{
//                     .class = class,
//                     .fields = std.StringHashMap(Value).init(allocator),
//                 };
//                 return instance;
//             }
//
//             pub fn deinit(self: *LoxInstance) void {
//                 self.fields.deinit();
//                 allocator.destroy(self);
//             }
//
//             pub fn get(self: *LoxInstance, name: Token) !Value {
//                 if (self.fields.get(name.lexeme)) |value| {
//                     return value;
//                 }
//
//                 if (self.class.methods.get(name.lexeme)) |method| {
//                     return Value{ .callable = &method.callable };
//                 }
//
//                 return error.UndefinedProperty;
//             }
//
//             pub fn set(self: *LoxInstance, name: Token, value: Value) void {
//                 self.fields.put(name.lexeme, value) catch @panic("Out of memory");
//             }
//
//             pub fn toString(self: *const LoxInstance) []const u8 {
//                 return std.fmt.allocPrint(allocator, "{s} instance", .{self.class.name}) catch @panic("Out of memory");
//             }
//         };
//     };
//
//     // ... (rest of the code remains unchanged)
// };
//
// // ... (tests remain unchanged)
//
//
