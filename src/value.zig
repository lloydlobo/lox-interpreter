const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;

const Expr = @import("expr.zig").Expr;
const Interpreter = @import("interpreter.zig");
const Token = @import("token.zig");
const formatNumber = @import("root.zig").formatNumber;
const logger = @import("logger.zig");
const root = @import("root.zig");

const Callable = @import("callable.zig");
const Function = @import("function.zig");

pub const Value = union(enum) {
    bool: bool,
    nil: void,
    num: f64,
    str: []const u8,

    /// The union represents return type of either `void` or `Value`.
    ret: *Return,

    /// Base `Expr.Call` interface or trait.
    callable: *CallableValue,

    /// Extends `LoxCallable`.
    function: *FunctionValue,

    /// Extends `LoxCallable`.
    class: *LoxClass,
    /// Extends `LoxClass` i.e. (the runtime representation of a class instance).
    instance: *LoxInstance,

    comptime {
        assert(@sizeOf(@This()) == 24);
        assert(@alignOf(@This()) == 8);
    }

    pub const Nil = Value{ .nil = {} };
    pub const True = Value{ .bool = true };
    pub const False = Value{ .bool = !true };

    pub inline fn from(x: anytype) Value {
        return switch (@TypeOf(x)) {
            usize, i32, comptime_int => Value{ .num = @as(f64, @floatFromInt(x)) },
            f64, comptime_float => Value{ .num = x },
            void => Nil,
            else => |@"type"| {
                logger.err(.{}, @src(), "Unimplemented case for type {any}.", .{@"type"});
                @panic("Unimplemented");
            },
        };
    }

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .bool => |bool_| try std.fmt.format(writer, "{}", .{bool_}),
            .nil => try std.fmt.format(writer, "nil", .{}),
            .num => |@"f64"| try formatNumber(writer, @"f64"),
            .str => |@"[]const u8"| try std.fmt.format(writer, "{s}", .{@"[]const u8"}),

            .ret => |ret| try std.fmt.format(writer, "{any}", .{ret}),

            .callable => |c| try std.fmt.format(writer, "{s}(<arity {d}>)", .{
                c.*.toString() catch |err| root.exit(.runtime_error, "{any}", .{err}),
                c.*.arity(),
            }),
            .function => |function| try std.fmt.format(writer, "{s}(<arity {d}>)", .{
                function.callable.toString() catch |err| root.exit(.runtime_error, "{any}", .{err}),
                function.callable.arity(),
            }),

            inline .class, .instance => |c| try std.fmt.format(
                writer,
                "{s}(<arity {d}>)",
                .{ c.*.toString(), c.*.arity() },
            ),
        }
    }

    pub const Return = union(enum) {
        nil: void,
        ret: Value,

        comptime {
            assert(@sizeOf(@This()) == 32);
            assert(@alignOf(@This()) == 8);
        }

        pub fn fromValue(value: ?Value) Return {
            return if (value) |v| switch (v) {
                .nil => .{ .ret = Value.Nil },
                else => |x| .{ .ret = x },
            } else .nil; //> .{ .ret = Value.Nil };
        }

        pub fn toValue(self: *Return) Value {
            return switch (self.*) {
                .nil => Value.Nil,
                .ret => |x| x,
            };
        }
    };

    pub const CallableValue = Callable;
    pub const FunctionValue = Function;

    // pub const LoxFunction = struct {
    //     /// `Context` pointer to hold function-specific data.
    //     context: *anyopaque,
    //
    //     // `LoxCallable` Traits
    //     arityFn: *const fn (*anyopaque) usize,
    //     callFn: *const fn (*anyopaque, *Interpreter, []Value) Value,
    //     toStringFn: *const fn (*anyopaque) []const u8,
    //
    //     comptime {
    //         assert(@sizeOf(@This()) == 32);
    //         assert(@alignOf(@This()) == 8);
    //     }
    //
    //     // `LoxCallable` Implementaion
    //     pub fn arity(self: *LoxFunction) usize {
    //         return self.arityFn(self.context);
    //     }
    //     pub fn call(self: *LoxFunction, interpreter: *Interpreter, arguments: []Value) Value {
    //         return self.callFn(self.context, interpreter, arguments);
    //     }
    //     pub fn toString(self: *LoxFunction) []const u8 {
    //         return self.toStringFn(self.context);
    //     }
    //
    //     // See https://craftinginterpreters.com/functions.html#function-objects
    // };

    /// The new classDecl rule relies on the function rule we defined
    /// earlier. To refresh your memory:
    ///      classDecl      → "class" IDENTIFIER "{" function* "}" ;
    ///      i.e.
    ///          funDecl        → "fun" function ;
    ///          function       → IDENTIFIER "(" parameters? ")" block ;
    ///          parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
    pub const LoxClass = struct {
        /// `Context` pointer to hold function-specific data.
        context: *anyopaque,

        // `LoxCallable` Traits
        arityFn: *const fn (*anyopaque) usize,
        callFn: *const fn (*anyopaque, *Interpreter, []Value) Value,
        toStringFn: *const fn (*anyopaque) []const u8,

        comptime {
            assert(@sizeOf(@This()) == 32);
            assert(@alignOf(@This()) == 8);
        }

        // `LoxCallable` Implementaion
        pub fn arity(self: *LoxClass) usize {
            return self.arityFn(self.context);
        }
        pub fn call(self: *LoxClass, interpreter: *Interpreter, arguments: []Value) Value {
            return self.callFn(self.context, interpreter, arguments);
        }
        pub fn toString(self: *LoxClass) []const u8 {
            return self.toStringFn(self.context);
        }
    };

    pub const LoxInstance = struct {
        /// `Context` pointer to hold function-specific data.
        context: *anyopaque,

        // `LoxCallable` Traits
        arityFn: *const fn (*anyopaque) usize,
        callFn: *const fn (*anyopaque, *Interpreter, []Value) Value,
        toStringFn: *const fn (*anyopaque) []const u8,

        // `LoxInstance` Properties
        get: *const fn (*anyopaque, Token) Value,

        comptime {
            // assert(@sizeOf(@This()) == 56);
            assert(@alignOf(@This()) == 8);
        }

        pub fn get(self: *LoxInstance, name: Token) Value {
            return self.get(self.context, name);
        }

        // `LoxCallable` Implementaion
        pub fn arity(self: *LoxInstance) usize {
            return self.arityFn(self.context);
        }
        pub fn call(self: *LoxInstance, interpreter: *Interpreter, arguments: []Value) Value {
            return self.callFn(self.context, interpreter, arguments);
        }
        pub fn toString(self: *LoxInstance) []const u8 {
            return self.toStringFn(self.context);
        }
    };
};
