const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const SourceLocation = std.builtin.SourceLocation;

const root = @import("root.zig");

const logger = @This();

comptime {
    assert(@sizeOf(@This()) == 0);
    assert(@alignOf(@This()) == 1);
}

const with_vertical_padding = false;
const with_horizontal_padding = true;

const v_pad = if (with_vertical_padding) "\n" else "";
const h_pad = if (with_horizontal_padding) "    " else "";

pub const color_reset = "\x1b[0m";
pub const color_bold = "\x1b[1m";
pub const color_white = "\x1b[37m";
pub const color_red = "\x1b[31m";

const glyph_color = color_white;
pub const indent = color_reset ++ "\t" ++ glyph_color ++ "└─ " ++ color_reset;
pub const newline: []const u8 = "\n" ++ indent;

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

    pub fn format(self: Scope, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
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

pub const Scoper = union(enum) {
    default,
    scope: Scope,

    pub fn makeScope(comptime src: std.builtin.SourceLocation) Scoper {
        return comptime .{ .scope = .{
            .name = src.fn_name,
            .parent = null,
        } };
    }

    pub fn withParent(self: Scoper, parent: *const Scope) Scoper {
        assert(self.scope.name.len > 0);
        return .{ .scope = .{
            .name = self.scope.name,
            .parent = parent,
        } };
    }

    pub fn toScope(self: Scoper) Scope {
        return switch (self) {
            .default => Scope{ .name = "default", .parent = null },
            .scope => |scope| scope,
        };
    }
};

test "Scoper" {
    try testing.expectEqual(Scoper.default, .default);
    try testing.expectEqual(
        Scoper{ .scope = .{ .name = "default", .parent = null } },
        Scoper{ .scope = .{ .name = "default", .parent = null } },
    );
}

fn countPlaceholders(comptime format: []const u8) usize {
    comptime {
        var count: usize = 0;
        var i: usize = 0;
        while (i < format.len) : (i += 1) {
            if (format[i] == '{') {
                if (i + 1 < format.len and format[i + 1] == '{') {
                    i += 1; // Skip escaped '{'
                } else {
                    count += 1;
                }
            }
        }

        return count;
    }
}

// STUB
// This depends on Zig compiler for the "referenced by: ` error logs to find `@src()`.
fn checkFormatArgs(comptime format: []const u8, comptime ArgsType: type) void {
    comptime {
        const expected_args = countPlaceholders(format);
        const provided_args = @typeInfo(ArgsType).Struct.fields.len;
        if (expected_args != provided_args) {
            @compileError(std.fmt.comptimePrint(
                "Argument count mismatch. Expected {d} arguments, but got {d}.\nFormat string: \"{s}\"",
                .{ expected_args, provided_args, format },
            ));
        }
    }
}

fn checkFormatArgsSrc(
    comptime format: []const u8,
    comptime ArgsType: type,
    comptime src: std.builtin.SourceLocation,
) void {
    comptime {
        const expected_args = countPlaceholders(format);
        const provided_args = @typeInfo(ArgsType).Struct.fields.len;
        if (expected_args != provided_args) {
            @compileError(std.fmt.comptimePrint(
                \\Error at {s}:{d}:{d} in function '{s}':
                \\Argument count mismatch. Expected {d} arguments, but got {d}.
                \\Format string: "{s}"
            , .{ src.file, src.line, src.column, src.fn_name, expected_args, provided_args, format }));
        }
    }
}

pub fn log(
    comptime level: LogLevel,
    scoper: Scoper,
    comptime src: SourceLocation,
    comptime format: []const u8,
    args: anytype,
) void {
    comptime {
        checkFormatArgsSrc(format, @TypeOf(args), src);

        const is_enabled = false;
        if (is_enabled) {
            const is_any_level_to_skip = (level == .info); // or level == .debug
            if (is_any_level_to_skip) {
                return;
            }
        }
    }

    // Strip the "src/" prefix from the source file path if it exists
    const is_starts_with_src = std.mem.startsWith(u8, src.file, "src/");
    const stripped_file: []const u8 = if (is_starts_with_src) src.file[4..] else src.file;

    // Buffer to capture the fully formatted scope (including parent scopes)
    var scope_buffer: [256]u8 = undefined; // note: resize as required
    var scope_stream = std.io.fixedBufferStream(&scope_buffer);
    const scope_writer = scope_stream.writer();

    // Format the scope into the buffer
    const scope: Scope = scoper.toScope();
    scope.format("", .{}, scope_writer) catch |e| {
        root.exit(.exit_failure, "Failed to format scope '{any}'", .{e});
    };
    const formatted_scope: []u8 = scope_buffer[0 .. scope_writer.context.getPos() catch |e| {
        root.exit(.exit_failure, "{any}", .{e});
    }];

    // Write the full log message
    root.stderr().writer().print(
        v_pad ++ h_pad ++
            "{s}{s}{s}{s}{s}: {s}:{s}{s}:{d}{s}:{d}:{s} {s}{s}:{s}{s} " ++
            format ++
            color_reset ++ "\n" ++ v_pad,
        .{
            glyph_color,      color_white,     formatted_scope,
            glyph_color,      color_reset,     stripped_file,
            color_white,      src.fn_name,     src.line,
            color_white,      src.column,      color_bold,
            level.getColor(), level.getName(), color_reset,
            color_bold,
        } ++ args,
    ) catch |e| {
        root.exit(.exit_failure, "Failed to print to stderr '{any}'", .{e});
    };
}

pub fn debug(scoper: Scoper, comptime src: SourceLocation, comptime format: []const u8, args: anytype) void {
    log(.debug, scoper, src, format, args);
}

pub fn info(scoper: Scoper, comptime src: SourceLocation, comptime format: []const u8, args: anytype) void {
    log(.info, scoper, src, format, args);
}

pub fn warn(scoper: Scoper, comptime src: SourceLocation, comptime format: []const u8, args: anytype) void {
    log(.warn, scoper, src, format, args);
}

pub fn err(scoper: Scoper, comptime src: SourceLocation, comptime format: []const u8, args: anytype) void {
    log(.err, scoper, src, format, args);
}

test "basic usage" {
    const skip_test = true;
    if (comptime skip_test) {
        return;
    }

    const root_scope: logger.Scoper = .{ .scope = .{
        .name = "app",
        .parent = null,
    } };
    const network_scope: logger.Scoper = .{ .scope = .{
        .name = "network",
        .parent = &root_scope.scope,
    } };
    const db_scope: logger.Scoper = .{ .scope = .{
        .name = "database",
        .parent = &root_scope.scope,
    } };

    logger.info(.default, @src(), "Starting application", .{});
    logger.debug(root_scope, @src(), "Starting application", .{});
    logger.info(network_scope, @src(), "Connected to server", .{});
    logger.warn(db_scope, @src(), "Slow query detected: {d}ms", .{150});
    logger.err(network_scope, @src(), "Connection lost: {s}", .{"Timeout"});

    logger.warn(.default, @src(),
        \\Resolving variable initializer expression.
        \\{s}variable: '{s}'."
    , .{ logger.indent, "bar_test" });
}
