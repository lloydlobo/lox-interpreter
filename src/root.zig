//! These functions are used throughout the codebase.

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = mem.Allocator;

const debug = @import("debug.zig");

pub const Code = u8;

pub const ErrorCode = enum(u8) {
    exit_success = 0,
    exit_failure = 1,
    syntax_error = 65,
    runtime_error = 70,

    pub fn fromInt(comptime T: type, error_code: u8) ?ErrorCode {
        assert(T == u8);
        return inline for (comptime std.meta.fields(ErrorCode)) |field| {
            if (error_code == @intFromEnum(@field(ErrorCode, field.name))) {
                return @field(ErrorCode, field.name);
            }
        } else null;
    }

    pub inline fn toString(comptime self: ErrorCode) []const u8 {
        comptime {
            return switch (self) {
                .exit_success => "exit success",
                .exit_failure => "exit failure",
                .syntax_error => "syntax error",
                .runtime_error => "runtime error",
            };
        }
    }
};

pub const debug_trace_flags: [8]bool = .{
    debug.is_trace_compiler,
    debug.is_trace_environment,
    debug.is_trace_garbage_collector,
    debug.is_trace_interpreter,
    debug.is_trace_parser,
    debug.is_trace_resolver,
    debug.is_trace_scanner,
    debug.is_trace_virtual_machine,
};

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
    assert(1 << 11 == 2048);
    assert(1 << 12 == 4096);
}

pub fn tracesrcLog(
    comptime message_level: std.log.Level,
    comptime src: std.builtin.SourceLocation,
    comptime fmt: []const u8,
    args: anytype,
) void {
    const src_fmt = "{s}{s}:{s}{s}:{d}{s}{s}:{d}{s}";

    const src_args = .{ COLOR_WHITE, src.file, COLOR_BOLD, src.fn_name, src.line, COLOR_RESET, COLOR_WHITE, src.column, COLOR_RESET };
    const message_color = switch (message_level) {
        .err => COLOR_RED,
        .warn => COLOR_YELLOW,
        .debug => COLOR_CYAN,
        .info => COLOR_GREEN,
    };
    const args_fmt = "\n" ++
        COLOR_YELLOW ++ "└─ " ++ message_color ++ fmt ++ COLOR_RESET;

    var buffer: [1 << 11]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();
    std.fmt.format(writer, src_fmt, src_args) catch |err| exit(.exit_failure, "{}", .{err});
    std.fmt.format(writer, args_fmt, args) catch |err| exit(.exit_failure, "{}", .{err});

    const pos = writer.context.*.getPos() catch |err| exit(.exit_failure, "{}", .{err});
    std.log.defaultLog(message_level, std.log.default_log_scope, "{s}", .{buffer[0..pos]});
}

/// NOTE: Manually change std.log.debug to std.log.warn to log in tests.
/// NOTE: `@src() std.builtin.SourceLocation` ─ Must be called in a function.
pub fn tracesrc(comptime src: anytype, comptime fmt: []const u8, args: anytype) void {
    if (comptime any(bool, debug_trace_flags, null)) {
        const src_fmt = "{s}{s}:{s}{s}:{d}{s}{s}:{d}{s}";
        const src_args = .{ COLOR_WHITE, src.file, COLOR_BOLD, src.fn_name, src.line, COLOR_RESET, COLOR_WHITE, src.column, COLOR_RESET };
        const args_fmt = "\n" ++ COLOR_YELLOW ++ "└─ " ++ COLOR_CYAN ++ fmt ++ COLOR_RESET;

        var buffer: [1 << 11]u8 = undefined;
        var stream = std.io.fixedBufferStream(&buffer);

        const writer = stream.writer();
        std.fmt.format(writer, src_fmt, src_args) catch |err| exit(.exit_failure, "{}", .{err});
        std.fmt.format(writer, args_fmt, args) catch |err| exit(.exit_failure, "{}", .{err});

        const pos = writer.context.*.getPos() catch |err| exit(.exit_failure, "{}", .{err});
        std.log.debug("{s}", .{buffer[0..pos]});
    }
}

// Copied from gitlab.com/andreorst/lox
pub fn exit(status: ErrorCode, comptime fmt: []const u8, args: anytype) noreturn {
    eprint(fmt, args);
    std.process.exit(@intFromEnum(status));
}

// See also https://stackoverflow.com/a/74187657
pub fn print(comptime fmt: []const u8, args: anytype) void {
    const writer = std.io.getStdOut().writer();
    writer.print(fmt, args) catch |err| {
        exit(.runtime_error, "Failed to write to stdout: {}", .{err});
    };
}

pub fn eprint(comptime fmt: []const u8, args: anytype) void {
    const writer = std.io.getStdErr().writer();
    writer.print(fmt, args) catch |err| {
        exit(.runtime_error, "Failed to write to stdout: {}", .{err});
    };
}

pub fn stdout() std.fs.File {
    return std.io.getStdOut();
}

pub fn stderr() std.fs.File {
    return std.io.getStdErr();
}

pub inline fn isAlphaNumeric(c: u8) bool {
    return switch (c) {
        '_', 'a'...'z', 'A'...'Z', '0'...'9' => true,
        inline else => false,
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

pub const LoxError = error{
    OutOfMemoryError,
    RuntimeError,
    CompileError,
};

/// Uses struct-like field access in a union (which asserts if you don't access the active tag).
/// May not be documented. E.g. you could write t.A to get the payload of the A variant, assuming it's the active tag. Currently using doing it via the @field builtin.
/// [See source link](https://github.com/ziglang/zig/issues/9271#issuecomment-871837227)
/// [See also](https://github.com/ziglang/zig/blob/7b5d139fd30a7225f073125b8a53e51a2454d223/lib/std/json.zig#L2811)
pub fn unionPayloadPtr(comptime T: type, union_ptr: anytype) ?*T {
    const U = @typeInfo(@TypeOf(union_ptr)).Pointer.child;

    const info = @typeInfo(U).Union;

    inline for (info.fields, 0..) |u_field, i| {
        if (u_field.type != T) {
            continue;
        }

        if (@intFromEnum(union_ptr.*) == i) {
            return &@field(union_ptr, u_field.name);
        }
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

/// Causes compile error when items are not indexable (e.g., array or slice).
/// Causes compile error when items do not have a known length.
pub inline fn any(comptime T: type, comptime items: anytype, comptime predicateFn: ?fn (T) bool) bool {
    return comptime blk: {
        if (std.meta.Elem(@TypeOf(items)) != T) {
            @compileError("items must be indexable (e.g., array or slice)");
        }
        // FIXME: Only checks .{} slices and not memory allocated ones.
        if (items.len == 0) {
            @compileError("items must have a known length");
        }
        for (items) |item| {
            if (predicateFn) |predicate| {
                if (predicate(item)) {
                    break :blk true;
                }
            } else switch (T) {
                bool => {
                    if (item) {
                        break :blk true;
                    }
                },
                inline else => break :blk false,
            }
        } else {
            break :blk false;
        }
    };
}

test "any ─ basic usage" {
    // zig fmt: off
    {
        const list = [_]i32{ 1, 2, 3, 4, 5 };
        try testing.expect(any(i32, list, struct { fn predicate(x: i32) bool { return x == 3; } }.predicate)); // assert that 3 is in the list
        try testing.expect(!any(i32, list, struct { fn predicate(x: i32) bool { return x == 6; } }.predicate)); // assert that 6 is not in the list
    }

    {
        const list = [_]bool{ false, false, true };
        try testing.expect(any(bool, list, null)); // assert that true is in the list
        try testing.expect(any(bool, list, struct { fn predicate(x: bool) bool { return x == false; } }.predicate)); // assert that false is in the list
    }

    {
        const list = [_][]const u8{ "apple", "banana", "cherry" };
        try testing.expect(any([]const u8, list, struct { fn predicate(x: []const u8) bool { return std.mem.eql(u8, x, "apple"); } }.predicate)); // assert that apple is in the list
        try testing.expect(any([]const u8, list, struct { fn predicate(x: []const u8) bool { return std.mem.eql(u8, x, "cherry"); } }.predicate)); // assert that cherry is not the list
    }
    // zig fmt: on
}

test "any ─ with custom structs" {
    const Point = struct {
        x: i32,
        y: i32,
    };

    const points = [_]Point{
        .{ .x = 1, .y = 2 },
        .{ .x = 3, .y = 4 },
        .{ .x = 5, .y = 6 },
    };

    // zig fmt: off
    try testing.expect(any(Point, &points, struct { fn predicate(p: Point) bool { return p.x == 3 and p.y == 4; } }.predicate));
    try testing.expect(!any(Point, &points, struct { fn predicate(p: Point) bool { return p.x == 7 and p.y == 8; } }.predicate));
    // zig fmt: on
}
test "any ─ with non-indexable type" { // This test should fail to compile
    if (comptime false) {
        const non_indexable = 42;
        _ = any(i32, non_indexable, null);
    }
}
test "any ─ with empty array" { // This test should fail to compile
    if (comptime false) {
        const empty_array = [_]i32{};
        _ = any(i32, &empty_array, null);
    }
}

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
