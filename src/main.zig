const std = @import("std");
const assert = std.debug.assert;

const AstPrinter = @import("astprinter.zig").AstPrinter;
const debug = @import("debug.zig");
const Interpreter = @import("interpreter.zig");
const Parser = @import("parser.zig");
const Resolver = @import("resolver.zig");
const root = @import("root.zig");
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("token.zig");

var g_had_runtime_error: bool = false;
var g_runtime_error_count: usize = 0;
var g_had_error: bool = false;
var g_error_count: usize = 0;

pub fn runtimeError(token: Token, comptime message: []const u8, args: anytype) void {
    std.debug.print(message ++ "\n[line {d}]\n", args ++ .{token.line});
    g_had_runtime_error = true;
    g_runtime_error_count += 1;
}

pub fn report(line: u32, comptime where: []const u8, comptime message: []const u8, args: anytype) void {
    std.debug.print("[line {d}] Error" ++ where ++ ": " ++ message ++ "\n", .{line} ++ args);
    g_had_error = true;
    g_error_count += 1;
}

pub fn @"error"(line: u32, comptime message: []const u8, args: anytype) void {
    report(line, "", message, args);
}

pub fn tokenError(token: Token, comptime message: []const u8) void {
    if (token.type == .eof) {
        report(token.line, " at end", message, .{});
    } else {
        report(token.line, " at '{s}'", message, .{token.lexeme});
    }
}

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
    blk: {
        const cmd = Command.fromString(command).?;
        var scanner = Scanner.init(file_contents, std.heap.page_allocator);
        defer scanner.deinit();

        const tokens = try scanner.scanTokens();

        if (cmd == .tokenize) {
            for (tokens) |token| try writer.print("{}\n", .{token});
            break :blk;
        }

        // `std.heap.ArenaAllocator` takes in a child allocator and allows you
        // to allocate many times and only free once. Here, .deinit() is called
        // on the arena, which frees all memory.
        //
        // Using allocator.free for further individual allocated memory would
        // be a no-op (i.e. does nothing).
        //
        // See https://zig.guide/standard-library/allocators
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        var parser = try Parser.init(tokens, allocator);

        if ((cmd == .parse) or (cmd == .evaluate)) {
            const expression = try parser.parseExpression();

            if (g_had_error) {
                break :blk; // stop if syntax error
            }

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
            if (g_had_error) {
                break :blk; // stop if syntax error
            }

            var interpreter = try Interpreter.init(allocator);
            // We do need to actually run the resolver, though. We insert the
            // new pass after the parser does its magic.
            var resolver = Resolver.init(allocator, &interpreter);
            try resolver.resolveStatements(statements);
            if (comptime debug.is_trace_interpreter) {
                var it = (try interpreter.locals.clone()).iterator();
                while (it.next()) |entry| {
                    root.tracesrc(@src(), "depth:'{any}','{}'", .{ entry.value_ptr.*, entry.key_ptr.* });
                }
            }
            try interpreter.interpret(statements, writer);
            break :blk;
        }
    }

    if (comptime debug.is_trace_interpreter) {
        root.tracesrc(@src(), "Encountered '{0d}' '{1s}' and '{2d}' '{3s}'.", .{
            g_runtime_error_count,
            root.ErrorCode.runtime_error.toString(),
            g_error_count,
            root.ErrorCode.syntax_error.toString(),
        });
    }

    var res = root.ErrorCode.exit_success;
    if (g_had_error) {
        res = .syntax_error;
    }
    if (g_had_runtime_error) {
        res = .runtime_error;
    }

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

    if (Command.fromString(command)) |cmd| {
        if (comptime debug.is_testing) {
            root.tracesrc(@src(), "Command: {}", .{cmd});
        }
    } else {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(@intFromEnum(root.ErrorCode.exit_failure));
    }
    assert(std.mem.endsWith(u8, filename, ".lox"));

    const file_contents = try std.fs.cwd().readFileAlloc(
        std.heap.page_allocator,
        filename,
        std.math.maxInt(usize),
    );
    defer std.heap.page_allocator.free(file_contents);

    const stdout_writer = std.io.getStdOut().writer();
    const exit_code = try run(
        stdout_writer,
        command,
        file_contents,
    );
    assert(root.ErrorCode.fromInt(u8, exit_code) != null);

    std.process.exit(exit_code);
}
