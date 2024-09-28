const std = @import("std");
const SourceLocation = std.builtin.SourceLocation;

const Logger = @This();

const with_vertical_padding = false;
const v_pad = if (with_vertical_padding) "\n" else "";
const with_horizontal_padding = true;
const h_pad = if (with_horizontal_padding) "    " else "";

pub const color_reset = "\x1b[0m";
pub const color_bold = "\x1b[1m";
pub const color_white = "\x1b[37m";
pub const color_red = "\x1b[31m";

const glyph_color = color_white;

pub const newline = "\n" ++ color_reset ++ "\t" ++ glyph_color ++ "└─ " ++ color_reset;

pub fn log(
    comptime level: LogLevel,
    scope: Scope,
    comptime src: SourceLocation,
    comptime format: []const u8,
    args: anytype,
) void {
    if (comptime level == .info) {
        return; // skip logging `LogLevel.info`
    }

    const stderr = std.io.getStdErr().writer();

    // Strip the "src/" prefix from the source file path if it exists
    const stripped_file = if (std.mem.startsWith(u8, src.file, "src/"))
        src.file[4..]
    else
        src.file;

    // Buffer to capture the fully formatted scope (including parent scopes)
    var scope_buffer: [256]u8 = undefined;
    var scope_stream = std.io.fixedBufferStream(&scope_buffer);
    const scope_writer = scope_stream.writer();

    // Format the scope into the buffer
    scope.format("", .{}, scope_writer) catch unreachable;

    // Write the full log message
    stderr.print(
        v_pad ++
            h_pad ++
            "{s}{s}{s}{s}{s}: {s}:{s}{s}:{d}{s}:{d}:{s} {s}{s}:{s}{s} " ++
            format ++
            color_reset ++
            "\n" ++
            v_pad,
        .{
            glyph_color,
            color_white,
            scope_buffer[0 .. scope_writer.context.getPos() catch unreachable],
            glyph_color,
            color_reset,
            stripped_file,
            color_white,
            src.fn_name,
            src.line,
            color_white,
            src.column,
            color_bold,
            level.getColor(),
            level.getName(),
            color_reset,
            color_bold,
        } ++ args,
    ) catch unreachable;
}

pub fn debug(scope: Scope, comptime src: SourceLocation, comptime format: []const u8, args: anytype) void {
    log(.debug, scope, src, format, args);
}

pub fn info(scope: Scope, comptime src: SourceLocation, comptime format: []const u8, args: anytype) void {
    log(.info, scope, src, format, args);
}

pub fn warn(scope: Scope, comptime src: SourceLocation, comptime format: []const u8, args: anytype) void {
    log(.warn, scope, src, format, args);
}

pub fn err(scope: Scope, comptime src: SourceLocation, comptime format: []const u8, args: anytype) void {
    log(.err, scope, src, format, args);
}

pub const LogLevel = enum {
    debug,
    info,
    warn,
    err,

    pub fn getColor(self: LogLevel) []const u8 {
        return switch (self) {
            .debug => "\x1b[36m", // Cyan
            .info => "\x1b[32m", // Green
            .warn => "\x1b[33m", // Yellow
            .err => "\x1b[31m", // Red
        };
    }

    pub fn getName(self: LogLevel) []const u8 {
        return @tagName(self);
    }
};

pub const Scope = struct {
    name: []const u8 = "default",
    parent: ?*const Scope = null,

    pub fn format(
        self: Scope,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        if (self.parent) |parent| {
            try parent.format("", .{}, writer);
            try writer.print(".{s}", .{self.name});
        } else {
            try writer.writeAll(self.name);
        }
    }
};

test "basic usage" {
    const root_scope = Scope{ .name = "app" };
    const network_scope = Scope{ .name = "network", .parent = &root_scope };
    const db_scope = Scope{ .name = "database", .parent = &root_scope };

    Logger.info(.{}, @src(), "Starting application", .{});
    Logger.debug(root_scope, @src(), "Starting application", .{});
    Logger.info(network_scope, @src(), "Connected to server", .{});
    Logger.warn(db_scope, @src(), "Slow query detected: {d}ms", .{150});
    Logger.err(network_scope, @src(), "Connection lost: {s}", .{"Timeout"});
}
