const std = @import("std");
const testing = std.testing;

const prog = @import("main.zig");

// see also https://app.codecrafters.io/courses/interpreter/stages/sc2?repo=22d0aedc-438f-4a1e-a88c-5736eb7b71db

const TestCase = struct {
    expected: []const u8,
    input: []const u8,
};

/// note that at this point multi line expression parsing is WIP
fn testParsingExpressions(comptime expected: []const u8, comptime input: []const u8) !void {
    var buf: [1024]u8 = undefined;
    var writer = std.io.fixedBufferStream(&buf);

    const exitcode = try prog.run(writer.writer(), "parse", input);
    const actual = writer.getWritten();

    try testing.expectEqualStrings(expected, actual);
    try testing.expectEqual(0, exitcode);
}

test "Parsing expressions - Booleans & Nil" {
    inline for ([_]TestCase{
        .{ .expected = "true", .input = "true" },
        .{ .expected = "false", .input = "false" },
        .{ .expected = "nil", .input = "nil" },
    }) |t| {
        try testParsingExpressions(t.expected, t.input);
    }
}

test "Parsing expressions - Number literals" {
    inline for ([_]TestCase{
        .{ .expected = "42.47", .input = "42.47" },
        .{ .expected = "46.0", .input = "46" },
        .{ .expected = "0.0", .input = "0.0" },
        .{ .expected = "73.83", .input = "73.83" },
    }) |t| {
        try testParsingExpressions(t.expected, t.input);
    }
}

test "Parsing expressions - String literals" {
    inline for ([_]TestCase{
        .{ .expected = "world hello", .input = "\"world hello\"" },
        .{ .expected = "'hello'", .input = "\"'hello'\"" },
        .{ .expected = "// quz", .input = "\"// quz\"" },
        .{ .expected = "18", .input = "\"18\"" },
    }) |t| {
        try testParsingExpressions(t.expected, t.input);
    }
}

test "Parsing expressions - Parentheses" {
    inline for ([_]TestCase{
        .{ .expected = "(group foo)", .input = "(\"foo\")" },
        .{ .expected = "(group (group true))", .input = "((true))" },
        .{ .expected = "(group nil)", .input = "(nil)" },
        .{ .expected = "(group 75.44)", .input = "(75.44)" },
    }) |t| {
        try testParsingExpressions(t.expected, t.input);
    }
}

test "Parsing expressions - Unary Operators" {
    inline for ([_]TestCase{
        .{ .expected = "(! false)", .input = "!false" },
        .{ .expected = "(- 50.0)", .input = "-50" },
        .{ .expected = "(! (! true))", .input = "!!true" },
        .{ .expected = "(group (! (! (group true))))", .input = "(!!(true))" },
    }) |t| {
        try testParsingExpressions(t.expected, t.input);
    }
}

test "Parsing expressions - Arithmetic operators (1/2)" {
    inline for ([_]TestCase{
        .{ .expected = "(/ (* 16.0 38.0) 58.0)", .input = "16 * 38 / 58" },
        .{ .expected = "(/ (* 71.0 81.0) 69.0)", .input = "71 * 81 / 69" },
        .{ .expected = "(/ (/ 12.0 46.0) 71.0)", .input = "12 / 46 / 71" },
        .{
            .expected = "(/ (* (* 63.0 59.0) 89.0) 92.0)",
            .input = "63 * 59 * 89 / 92",
        },
        .{
            .expected = "(group (/ (* 22.0 (- 20.0)) (group (* 66.0 65.0))))",
            .input = "(22 * -20 / (66 * 65))",
        },
    }) |t| {
        try testParsingExpressions(t.expected, t.input);
    }
}

test "Parsing expressions - Arithmetic operators (2/2)" {
    inline for ([_]TestCase{
        .{ .expected = "(+ hello world)", .input = "\"hello\" + \"world\"" },
        .{ .expected = "(- (- 76.0 22.0) 32.0)", .input = "76 - 22 - 32" },
        .{ .expected = "(- (+ 65.0 50.0) 55.0)", .input = "65 + 50 - 55" },
        .{
            .expected = "(/ (* (group (+ (- 63.0) 28.0)) (group (* 36.0 29.0))) (group (+ 98.0 74.0)))",
            .input = "(-63 + 28) * (36 * 29) / (98 + 74)",
        },
    }) |t| {
        try testParsingExpressions(t.expected, t.input);
    }
}

test "Parsing expressions - Comparisons operators" {
    inline for ([_]TestCase{
        .{ .expected = "(> 98.0 (- 1.0))", .input = "98 > -1" },
        .{ .expected = "(<= 99.0 197.0)", .input = "99 <= 197" },
        .{ .expected = "(< (< 98.0 197.0) 296.0)", .input = "98 < 197 < 296" },
        .{
            .expected = "(>= (group (- 43.0 28.0)) (- (group (+ (/ 32.0 65.0) 62.0))))",
            .input = "(43 - 28) >= -(32 / 65 + 62)",
        },
    }) |t| {
        try testParsingExpressions(t.expected, t.input);
    }
}

test "Parsing expressions - Equality operators" {
    inline for ([_]TestCase{
        .{ .expected = "(!= hello world)", .input = "\"hello\"!=\"world\"" },
        .{ .expected = "(== foo foo)", .input = "\"foo\" == \"foo\"" },
        .{ .expected = "(== 31.0 94.0)", .input = "31 == 94" },
        .{
            .expected = "(== (group (!= 76.0 17.0)) (group (>= (group (+ (- 87.0) 39.0)) (group (* 83.0 29.0)))))",
            .input = "(76 != 17) == ((-87 + 39) >= (83 * 29))",
        },
    }) |t| {
        try testParsingExpressions(t.expected, t.input);
    }
}

//
//
// Syntax Errors
//
//
//

const TestCaseSyntaxErrors = struct {
    expected: u8 = @intFromEnum(prog.ErrorCode.syntax_error),
    input: []const u8,
};

fn testParsingExpressionsSyntaxErrors(comptime expected: u8, comptime input: []const u8) !void {
    var buf: [1024]u8 = undefined;
    var writer = std.io.fixedBufferStream(&buf);

    const exitcode = try prog.run(writer.writer(), "parse", input);
    try testing.expectEqual(expected, exitcode);
}

test "Parsing expressions - Syntactic errors" {
    inline for ([_]TestCaseSyntaxErrors{
        .{ .input = "\"foo" },
        .{ .input = "(bar" },
        .{ .input = "(72 +)" },
        .{ .input = "+" },
    }) |t| {
        try testParsingExpressionsSyntaxErrors(t.expected, t.input);
    }
}
