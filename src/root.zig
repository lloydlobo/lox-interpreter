//! These functions are used throughout the codebase.

const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;

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

pub fn concatStrings(allocator: Allocator, left: []const u8, right: []const u8) ![]u8 {
    const nl = left.len;
    const nr = right.len;
    assert(nl > 0 and nr > 0);

    var buffer = try allocator.alloc(u8, (nl + nr));
    errdefer allocator.free(buffer);

    std.mem.copyForwards(u8, buffer[0..nl], right);
    std.mem.copyForwards(u8, buffer[nl..], right);

    return buffer;
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
