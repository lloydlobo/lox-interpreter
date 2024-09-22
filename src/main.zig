const std = @import("std");
const assert = std.debug.assert;

const AstPrinter = @import("astprinter.zig").AstPrinter;
const Interpreter = @import("interpreter.zig");
const Parser = @import("parser.zig");
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("token.zig");

/// Toggled by `runtimeError()`
var g_had_runtime_error: bool = false;

/// Toggled by `runtimeError()`
var g_runtime_error_count: usize = 0;

/// Toggled by `report()`
var g_had_error: bool = false;

/// Toggled by `report()`
var g_error_count: usize = 0;

pub fn runtimeError(token: Token, comptime message: []const u8, args: anytype) void {
    std.debug.print(message ++ "\n[line {}]\n", args ++ .{token.line});
    g_had_runtime_error = true;
    g_runtime_error_count += 1;
}

pub fn report(line: u32, comptime where: []const u8, comptime message: []const u8, args: anytype) void {
    std.debug.print("[line {}] Error" ++ where ++ ": " ++ message ++ "\n", .{line} ++ args);
    g_had_error = true;
    g_error_count += 1;
}

pub fn @"error"(line: u32, comptime message: []const u8, args: anytype) void {
    report(line, "", message, args);
}

// See also https://craftinginterpreters.com/parsing-expressions.html#entering-panic-mode
pub fn tokenError(token: Token, comptime message: []const u8) void {
    if (token.type == .eof) {
        report(token.line, " at end", message, .{});
    } else {
        report(token.line, " at '{s}'", message, .{token.lexeme});
    }
}

pub const ErrorCode = enum(u8) {
    no_error = 0,
    syntax_error = 65,
    runtime_error = 70,

    pub fn toString(self: ErrorCode) []const u8 {
        return switch (self) {
            .no_error => "No Error",
            .syntax_error => "Syntax Error",
            .runtime_error => "Runtime Error",
        };
    }
};

pub const Command = enum {
    tokenize,
    parse,
    evaluate,
    run,

    pub fn fromString(s: []const u8) ?Command {
        return inline for (comptime std.meta.fieldNames(Command)) |field| {
            if (std.mem.eql(u8, s, field))
                return @field(Command, field);
        } else null;
    }

    pub fn toString(self: Command) []const u8 {
        return @tagName(self);
    }
};

pub fn run(writer: anytype, command: []const u8, file_contents: []const u8) !u8 {
    // var bw = std.io.bufferedWriter(writer);
    // const stdout_writer = bw.writer(); // const writer = std.io.getStdOut().writer();

    blk: {
        const cmd = Command.fromString(command).?;
        var scanner = Scanner.init(file_contents, std.heap.page_allocator);
        defer scanner.deinit();

        const tokens = try scanner.scanTokens();

        if (cmd == .tokenize) {
            for (tokens) |token| try writer.print("{}\n", .{token});
            break :blk;
        }

        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const allocator = arena.allocator();

        var parser = try Parser.init(tokens, allocator);

        if ((cmd == .parse) or (cmd == .evaluate)) {
            const expression = try parser.parseExpression();

            if (g_had_error)
                break :blk; // stop if syntax error

            if (cmd == .parse) {
                try AstPrinter.print(writer, expression.?);
                break :blk;
            }

            if (cmd == .evaluate) {
                var interpreter = try Interpreter.init(allocator);
                try interpreter.interpretExpression(expression.?, writer);
                break :blk;
            }
        }

        if (cmd == .run) {
            const statements = try parser.parse();
            if (g_had_error)
                break :blk; // stop if syntax error

            // DEBUGGING
            if (false) { // stack alloc error when Stmt.if_stmt{...} is used
                for (statements) |stmt| std.debug.print("{}\n", .{stmt});
            }

            var interpreter = try Interpreter.init(arena.allocator());
            try interpreter.interpret(statements, writer);
            break :blk;
        }
    }

    if (false)
        std.debug.print("Encountered {0d} {1s} and {2d} {3s}.\n", .{
            g_runtime_error_count,
            ErrorCode.runtime_error.toString(),
            g_error_count,
            ErrorCode.syntax_error.toString(),
        });

    // try bw.flush();

    var res: ErrorCode = .no_error;
    if (g_had_error) res = .syntax_error;
    if (g_had_runtime_error) res = .runtime_error;

    return @intFromEnum(res);
}

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const command = args[1];
    const filename = args[2];

    if (Command.fromString(command) == null) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }

    assert(std.mem.endsWith(u8, filename, ".lox"));

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    const stdout_file = std.io.getStdOut().writer();
    const exit_code: u8 = try run(stdout_file, command, file_contents);

    assert(
        exit_code == @intFromEnum(ErrorCode.no_error) //
        or exit_code == @intFromEnum(ErrorCode.syntax_error) //
        or exit_code == @intFromEnum(ErrorCode.runtime_error), //
    );

    std.process.exit(exit_code);
}
