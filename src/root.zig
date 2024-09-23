//! These functions are used throughout the codebase.

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const debug = @import("debug.zig");

// ANSI color codes
const COLOR_RESET = "\x1b[0m";
const COLOR_BOLD = "\x1b[1m";
const COLOR_RED = "\x1b[31m";
const COLOR_GREEN = "\x1b[32m";
const COLOR_YELLOW = "\x1b[33m";
const COLOR_BLUE = "\x1b[34m";
const COLOR_CYAN = "\x1b[36m";
const COLOR_WHITE = "\x1b[37m";

comptime {
    assert(1 << 1 == 2);
    assert(1 << 2 == 4);
    assert(1 << 3 == 8);
    assert(1 << 4 == 16);
    assert(1 << 5 == 32);
    assert(1 << 6 == 64);
    assert(1 << 7 == 128);
    assert(1 << 8 == 256);
    assert(1 << 9 == 512);
    assert(1 << 10 == 1024);
}

/// src: `@src() std.builtin.SourceLocation` - Must be called in a function.
pub fn tracesrc(comptime src: anytype, comptime fmt: []const u8, args: anytype) void {
    if (comptime (debug.is_trace_env or
        debug.is_trace_gc or
        debug.is_trace_parser or
        debug.is_trace_scanner or
        debug.is_trace_vm or
        debug.is_trace_compiler))
    {
        var buffer: [1 << 10]u8 = undefined;
        var stream = std.io.fixedBufferStream(&buffer);
        const writer = stream.writer();
        {
            std.fmt.format(writer, "{s}{s}:{s}:{d}:{d}{s}", .{ COLOR_BOLD, src.file, src.fn_name, src.line, src.column, COLOR_RESET }) catch |err| exit(1, "{}", .{err});
            const colored_fmt = "\n\t" ++ COLOR_CYAN ++ "└─ " ++ COLOR_BLUE ++ fmt ++ COLOR_RESET;
            std.fmt.format(writer, colored_fmt, args) catch |err| exit(1, "{}", .{err});
        }

        std.log.debug("{s}", .{buffer[0 .. writer.context.*.getPos() catch |err| exit(1, "{}", .{err})]});
    }
}

// Copied from gitlab.com/andreorst/lox
pub fn exit(status: u8, comptime fmt: []const u8, args: anytype) noreturn {
    eprint(fmt, args);
    std.process.exit(status);
}

// See also https://stackoverflow.com/a/74187657
pub fn print(comptime fmt: []const u8, args: anytype) void {
    const w = std.io.getStdOut().writer();
    w.print(fmt, args) catch |err| exit(100, "Failed to write to stdout: {}", .{err});
}

pub fn eprint(comptime fmt: []const u8, args: anytype) void {
    const w = std.io.getStdErr().writer();
    w.print(fmt, args) catch |err| exit(100, "Failed to write to stdout: {}", .{err});
}

pub fn stdout() std.fs.File {
    return std.io.getStdOut();
}

pub fn stderr() std.fs.File {
    return std.io.getStdErr();
}

pub fn isAlphaNumeric(c: u8) bool {
    return switch (c) {
        '_', 'a'...'z', 'A'...'Z', '0'...'9' => true,
        else => false,
    };
}

pub fn formatNumber(writer: anytype, num: f64) !void {
    assert(1 << 8 == (1024 / 4)); // 256
    assert(1 << 12 == (1024 * 4)); // 4096

    var buf: [1 << 8]u8 = undefined;
    const str = try std.fmt.bufPrint(&buf, "{d}", .{num});

    try writer.writeAll(str);
    if (std.mem.indexOfScalar(u8, str, '.') == null)
        try writer.writeAll(".0");
}

// See https://gitlab.com/andreyorst/lox/-/blob/main/src/zig/lox/common.zig?ref_type=heads#L80
pub fn typeNameUnqualified(comptime T: type) []const u8 {
    const name = @typeName(T); //> *const [N:0]u8
    const index: comptime_int = if (std.mem.lastIndexOfAny(u8, name, ".")) |i| (i + 1) else 0;

    return name[index..];
}

/// Uses struct-like field access in a union (which asserts if you don't access
/// the active tag).
/// May not be documented. E.g. you could write t.A to get the payload of the A
/// variant, assuming it's the active tag. Currently using doing it via the
/// @field builtin.
/// [See source link](https://github.com/ziglang/zig/issues/9271#issuecomment-871837227)
/// [See also](https://github.com/ziglang/zig/blob/7b5d139fd30a7225f073125b8a53e51a2454d223/lib/std/json.zig#L2811)
pub fn unionPayloadPtr(comptime T: type, union_ptr: anytype) ?*T {
    const U = @typeInfo(@TypeOf(union_ptr)).Pointer.child;
    const info = @typeInfo(U).Union;
    inline for (info.fields, 0..) |u_field, i| {
        if (u_field.type != T) continue;
        if (@intFromEnum(union_ptr.*) == i) return &@field(union_ptr, u_field.name);
    }

    return null;
}

test "unionPayloadPtr" {
    const Tagged = union(enum) {
        A: i32,
        B: []const u8,
        C: i32,
    };

    var t1: Tagged = .{ .C = -5 };
    if (unionPayloadPtr(i32, &t1)) |ptr| try testing.expectEqual(-5, ptr.*);
    if (unionPayloadPtr(i32, &t1)) |ptr| ptr.* = 100;
    if (unionPayloadPtr(i32, &t1)) |ptr| try testing.expectEqual(100, ptr.*);

    var t2: Tagged = .{ .B = "hello" };
    if (unionPayloadPtr([]const u8, &t2)) |ptr| try testing.expectEqualStrings("hello", ptr.*);
    if (unionPayloadPtr([]const u8, &t2)) |ptr| ptr.*.ptr = "world";
    if (unionPayloadPtr([]const u8, &t2)) |ptr| try testing.expectEqualStrings("world", ptr.*);
}

pub const LoxError = error{
    OutOfMemoryError,
    RuntimeError,
    CompileError,
};

// See https://stackoverflow.com/a/66665672
//
// Array concatenation operator, for two comptime-known strings:
//
// const final_url = "https://github.com/" ++ user ++ "/reponame";
// std.fmt.comptimePrint for comptime-known strings and numbers and other formattable things:
//
// const final_url = comptime std.fmt.comptimePrint("https://github.com/{s}/reponame", .{user});
// Runtime, with allocation:
//
// const final_url = try std.fmt.allocPrint(alloc, "https://github.com/{s}/reponame", .{user});
// defer alloc.free(final_url);
// Runtime, no allocation, with a comptime-known maximum length:
//
// var buffer = [_]u8{undefined} ** 100;
// const printed = try std.fmt.bufPrint(&buffer, "https://github.com/{s}/reponame", .{user});
// Runtime, using ArrayList
//
// var string = std.ArrayList(u8).init(gpa);
// defer string.deinit();
// try string.appendSlice("https://github.com/");
// try string.appendSlice(user);
// try string.appendSlice("/reponame");
// const final_url = string.items;

//
// const Writer = struct {
//     writeFn: *const fn (self: *Writer, bytes: []const u8) anyerror!usize,
//
//     pub fn write(self: *Writer, bytes: []const u8) anyerror!usize {
//         return self.writeFn(self, bytes);
//     }
// };
