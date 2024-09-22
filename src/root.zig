//! These functions are used throughout the codebase.

const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;

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
