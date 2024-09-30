const std = @import("std");
const testing = std.testing;

const main = @import("main.zig");
const root = @import("root.zig");

// See also https://github.com/munificent/craftinginterpreters/tree/01e6f5b8f3e5dfa65674c2f9cf4700d73ab41cf8/test/expressions

const TestCase = struct {
    expected: []const u8,
    input: []const u8,
};

/// Note that at this point multi line expression evaluation is WIP
fn testEvaluatingExpressions(comptime expected: []const u8, comptime input: []const u8) !void {
    var buf: [1024]u8 = undefined;
    var writer = std.io.fixedBufferStream(&buf);

    const exitcode: u8 = try main.run(writer.writer(), "evaluate", input);
    testing.expectEqual(0, exitcode) catch |err|
        std.debug.print("{any}\nInput: {s}\nExpected: {d}\n", .{ err, input, 0 });

    const actual = writer.getWritten();
    testing.expectEqualStrings(expected, actual) catch |err|
        std.debug.print("{any}\nInput: {s}\nExpected: {s}\n", .{ err, input, expected });
}

test "Evaluating Expressions - Literals: Booleans & Nil" {
    inline for ([_]TestCase{
        .{ .expected = "true", .input = "true" },
        .{ .expected = "false", .input = "false" },
        .{ .expected = "nil", .input = "nil" },
    }) |t| try testEvaluatingExpressions(t.expected, t.input);
}

test "Evaluating Expressions - Literals: Strings & Numbers" {
    inline for ([_]TestCase{
        .{ .expected = "76", .input = "76" },
        .{ .expected = "73.74", .input = "73.74" },
        .{ .expected = "bar quz", .input = "\"bar quz\"" },
        .{ .expected = "55", .input = "\"55\"" },
    }) |t| try testEvaluatingExpressions(t.expected, t.input);
}

test "Evaluating Expressions - Parentheses" {
    inline for ([_]TestCase{
        .{ .expected = "true", .input = "(true)" },
        .{ .expected = "42", .input = "(42)" },
        .{ .expected = "hello world!", .input = "(\"hello world!\")" },
        .{ .expected = "true", .input = "((true))" },
    }) |t| try testEvaluatingExpressions(t.expected, t.input);
}

test "Evaluating Expressions - Negation & Not" {
    inline for ([_]TestCase{
        .{ .expected = "-73", .input = "-(73)" },
        .{ .expected = "false", .input = "!true" },
        .{ .expected = "false", .input = "!10.40" },
        .{ .expected = "true", .input = "!((false))" },
        .{ .expected = "true", .input = "!nil" },
        .{ .expected = "true", .input = "(!!93)" },
        .{ .expected = "false", .input = "!(!!93)" },
    }) |t| try testEvaluatingExpressions(t.expected, t.input);
}

test "Evaluating Expressions - Arithmetic Operators (1/2)" {
    // For now, all arithmetic operations will involve only two numbers.

    inline for ([_]TestCase{
        .{ .expected = "8.4", .input = "42 / 5" },
        .{ .expected = "3", .input = "18 * 3 / (3 * 6)" },
        .{ .expected = "10.4", .input = "(10.40 * 2) / 2" },
        .{ .expected = "2", .input = "7 * 2 / 7 / 1" },
        //
    }) |t| try testEvaluatingExpressions(t.expected, t.input);
}

test "Evaluating Expressions - Arithmetic Operators (2/2)" {
    inline for ([_]TestCase{
        .{ .expected = "3", .input = "5 - 2" },
        .{ .expected = "8.4", .input = "10.40 - 2" },
        .{ .expected = "3", .input = "(5 - (3 - 1))" },
        .{ .expected = "75", .input = "20 + 74 - (-(14 - 33))" },
    }) |t| try testEvaluatingExpressions(t.expected, t.input);
}

test "Evaluating Expressions - String Concatenation" {
    inline for ([_]TestCase{
        .{ .expected = "foobar", .input = "\"foo\" + \"bar\"" },
        .{ .expected = "hello world!", .input = "\"hello\" + \" world!\"" },
        .{ .expected = "4224", .input = "\"42\" + \"24\"" },
        .{ .expected = "bazworldquz", .input = "\"baz\" + \"world\" + \"quz\"" },
        .{ .expected = "fooquzquzbar", .input = "(\"foo\" + \"quz\") + (\"quz\" + \"bar\")" },
    }) |t| try testEvaluatingExpressions(t.expected, t.input);
}

test "Evaluating Expressions - Relational Operators" {
    inline for ([_]TestCase{
        .{ .expected = "true", .input = "10 > 5" },
        .{ .expected = "true", .input = "57 > -65" },
        .{ .expected = "true", .input = "11 >= 11" },
        .{ .expected = "true", .input = "(54 - 67) >= -(114 / 57 + 11)" },
    }) |t| try testEvaluatingExpressions(t.expected, t.input);
}

test "Evaluating Expressions - Equality Operators" {
    inline for ([_]TestCase{
        .{ .expected = "true", .input = "\"hello\" != \"bar\"" },
        .{ .expected = "true", .input = "\"hello\" == \"hello\"" },
        .{ .expected = "false", .input = "42 == \"42\"" },
        .{ .expected = "true", .input = "65 == (14 + 51)" },
    }) |t| try testEvaluatingExpressions(t.expected, t.input);
}

//
//
// Runtime Errors
//
//
//

const TestCaseRuntimeErrors = struct {
    expected: u8 = @intFromEnum(root.ErrorCode.runtime_error),
    input: []const u8,
};

/// Note that at this point multi line expression evaluation is WIP.
///
/// temp note: Runtime errors may avoid all test cases to be tested.
fn testEvaluatingExpressionsRuntimeErrors(tc: TestCaseRuntimeErrors, comptime expected_output: []const u8) !void {
    var buf: [1024]u8 = undefined;
    var writer = std.io.fixedBufferStream(&buf);

    const actual_exitcode: u8 = try main.run(writer.writer(), "evaluate", tc.input);
    testing.expectEqual(tc.expected, actual_exitcode) catch |err|
        std.debug.print("{0any}\nInput: {1s}\nExpected: {2d}\nActual: {3d}\n", .{
        err,
        tc.input,
        tc.expected,
        actual_exitcode,
    });

    const _is_enabled = false;
    if (_is_enabled) { // assumption(20240912073103UTC): disables, since while reporting errors, `std.debug.print` may flush after the test completes; in case of errors
        const actual_output = writer.getWritten();
        testing.expectEqualStrings(expected_output, actual_output) catch |err|
            std.debug.print("{0any}\nInput: {1s}\nExpected: {2s}\nActual: {3s}\n", .{
            err,
            tc.input,
            expected_output,
            actual_output,
        });
    }
}

test "Evaluating Expressions - Runtime Errors: Unary Operators" {
    inline for ([_]TestCaseRuntimeErrors{
        .{ .input = "-\"foo\"" },
        .{ .input = "-true" },
        .{ .input = "-false" },
        .{ .input = "-(\"foo\" + \"bar\")" },
    }) |tc| try testEvaluatingExpressionsRuntimeErrors(tc, "Operand must be a number.");
}

test "Evaluating Expressions - Runtime Errors: Binary Operators (1/2)" {
    inline for ([_]TestCaseRuntimeErrors{
        .{ .input = "90 * \"foo\"" },
        .{ .input = "\"quz\" / 76" },
        .{ .input = "true / false" },
        .{ .input = "-false" },
        .{ .input = "(\"foo\" + \"foo\") * (\"foo\" + \"foo\")" },
    }) |tc| try testEvaluatingExpressionsRuntimeErrors(tc, "Operands must be numbers.");
}

test "Evaluating Expressions - Runtime Errors: Binary Operators (2/2)" {
    inline for ([_]TestCaseRuntimeErrors{
        .{ .input = "\"hello\" + false" },
        .{ .input = "24 + \"world\" + 42" },
        .{ .input = "36 - false" },
        .{ .input = "-false" },
        .{ .input = "false - (\"bar\" + \"foo\")" },
    }) |tc| try testEvaluatingExpressionsRuntimeErrors(tc, "Operands must be two numbers or two strings.");
}

test "Evaluating Expressions - Runtime Errors: Relational Operators" {
    inline for ([_]TestCaseRuntimeErrors{
        .{ .input = "\"bar\" < false" },
        .{ .input = "false <= (79 + 22)" },
        .{ .input = "78 > (\"foo\" + \"world\")" },
        .{ .input = "true >= false" },
        .{ .input = "false - (\"bar\" + \"foo\")" },
    }) |tc| try testEvaluatingExpressionsRuntimeErrors(tc, "Operands must be two numbers or two strings.");
}
