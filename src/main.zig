const std = @import("std");
const assert = std.debug.assert;

const AstPrinter = @import("astprinter.zig");
const Interpreter = @import("interpreter.zig");
const Parser = @import("parser.zig");
const Resolver = @import("resolver.zig");
const Scanner = @import("scanner.zig");
const Token = @import("token.zig");
const debug = @import("debug.zig");
const logger = @import("logger.zig");
const root = @import("root.zig");

// # NOTES
//
// ## Interpreter
//
// * Using a more structured error handling approach instead of global variables.
// * Ensuring proper memory management for all allocated objects.
// * Implementing a more direct method for handling function returns.
// * Ensuring the resolver is properly integrated into the interpretation process.
//
// ## Resolver
//
// * Consider unifying the error handling approach between the resolver and interpreter.
// * Consider making the handling of global variables more explicit in both the resolver and interpreter.

/// Toggle flag for new work-in-progress features.
///
/// * 20240927040854UTC
///   https://craftinginterpreters.com/resolving-and-binding.html#static-scope
pub const g_is_stable_pre_resolver_feature_flag = false;

// Just use this to read, and not edit globally but from functions in `main.zig`.
pub var g_had_runtime_error: bool = false;
pub var g_runtime_error_count: usize = 0;
pub var g_had_error: bool = false;
pub var g_error_count: usize = 0;

pub fn runtimeError(token: Token, comptime message: []const u8, args: anytype) void {
    root.eprint(message ++ "\n[line {d}]\n", args ++ .{token.line});
    g_had_runtime_error = true;
    g_runtime_error_count += 1;
}

pub fn report(line: u32, comptime where: []const u8, comptime message: []const u8, args: anytype) void {
    root.eprint("[line {d}] Error" ++ where ++ ": " ++ message ++ "\n", .{line} ++ args);
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
            if (comptime !g_is_stable_pre_resolver_feature_flag) {
                // We do need to actually run the resolver, though.
                // We insert the new pass after the parser does its magic.
                var resolver = Resolver.init(allocator, &interpreter);
                try resolver.resolveStatements(statements);
                if (g_had_error) {
                    break :blk;
                }
                root.printStringHashMap(interpreter.locals);
                root.printStringHashMap(interpreter.environment.values);
                root.printStringHashMap(interpreter.globals.values);
            }
            try interpreter.interpret(statements, writer);

            break :blk;
        }
    }

    if (comptime debug.is_trace_interpreter) {
        const total_errors = g_runtime_error_count + g_error_count;
        if (total_errors != 0) {
            logger.err(.default, @src(), "Found {d} error(s).{s}{s}: {d}{s}{s}: {d}", //
                .{ total_errors, logger.newline, root.ErrorCode.runtime_error.toString(), g_runtime_error_count, logger.newline, root.ErrorCode.syntax_error.toString(), g_error_count });
        }
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
    logger.debug(.default, @src(), "Process args: {s} {s}", .{ logger.newline, args });

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const command = args[1];
    const filename = args[2];

    if (Command.fromString(command)) |cmd| {
        if (comptime debug.is_testing) {
            logger.info(.default, @src(), "Command: {}", .{cmd});
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
    assert(root.ErrorCode.fromInt(exit_code) != null);

    std.process.exit(exit_code);
}
