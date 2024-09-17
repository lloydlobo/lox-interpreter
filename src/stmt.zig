const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const Expr = @import("expr.zig").Expr;
const Interpreter = @import("interpreter.zig").Interpreter;
const Token = @import("token.zig").Token;

pub const Stmt = union(enum) {
    expr: *Expr,
    @"var": struct {
        /// The identifier.
        name: Token,
        initializer: ?*Expr,
    },
    print: *Expr,
    block: []Stmt,

    /// The statement analogue to the `evaluate()` method we have for
    /// expressions. Since we’re working with lists now, we need to let Zig know.
    pub fn accept(stmt: Stmt, interpreter: *Interpreter, writer: anytype) Interpreter.Error!Expr.Value {
        switch (stmt) {
            .block => |block| {
                _ = block;
                @panic("Unimplemented");
            },
            .expr => |expr| {
                _ = try interpreter.evaluate(expr);
                @panic("Unimplemented");
            },
            .print => |expr| {
                const value = try interpreter.evaluate(expr);
                Interpreter.printValue(writer, value);
                writer.writeByte('\n') catch {};
                // switch (value) {
                //     .str => |x| interpreter.allocator.free(x), // avoid recursion by discarding processed value
                //     else => {},
                // }
            },
            .@"var" => |@"var"| {
                _ = @"var";
                @panic("Unimplemented");
            },
        }

        return .{ .nil = {} };
    }
};
