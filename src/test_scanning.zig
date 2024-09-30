const std = @import("std");
const testing = std.testing;

const main = @import("main.zig");
const root = @import("root.zig");

const TestCase = struct {
    name: []const u8,
    input: []const u8,
    expected: []const u8,

    pub fn format(self: TestCase, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "Test Case: {s}\nInput:\n{s}\nExpected:\n{s}", .{
            self.name, self.input, self.expected,
        });
    }
};

//[See also](https://github.com/munificent/craftinginterpreters/tree/01e6f5b8f3e5dfa65674c2f9cf4700d73ab41cf8/test/scanning)
test "Test Scanning -> function run -> command: tokenize" {
    const test_cases = [_]TestCase{
        .{
            .name = "Empty file",
            .input = "\n",
            .expected = "EOF  null\n",
        },

        .{
            .name = "Parantheses",
            .input = "(()\n",
            .expected = "LEFT_PAREN ( null\nLEFT_PAREN ( null\nRIGHT_PAREN ) null\nEOF  null\n",
        },
        .{
            .name = "Parantheses",
            .input = "(\n",
            .expected = "LEFT_PAREN ( null\nEOF  null\n",
        },
        .{
            .name = "Parantheses",
            .input = "()()(\n",
            .expected = "LEFT_PAREN ( null\nRIGHT_PAREN ) null\nLEFT_PAREN ( null\nRIGHT_PAREN ) null\nLEFT_PAREN ( null\nEOF  null\n",
        },
        .{
            .name = "Parantheses",
            .input = ")((()()\n",
            .expected = "RIGHT_PAREN ) null\nLEFT_PAREN ( null\nLEFT_PAREN ( null\nLEFT_PAREN ( null\nRIGHT_PAREN ) null\nLEFT_PAREN ( null\nRIGHT_PAREN ) null\nEOF  null\n",
        },

        .{
            .name = "Braces",
            .input = "}\n",
            .expected = "RIGHT_BRACE } null\nEOF  null\n",
        },
        .{
            .name = "Braces",
            .input = "{{}}\n",
            .expected = "LEFT_BRACE { null\nLEFT_BRACE { null\nRIGHT_BRACE } null\nRIGHT_BRACE } null\nEOF  null\n",
        },
        .{
            .name = "Braces",
            .input = "{}{}{\n",
            .expected = "LEFT_BRACE { null\nRIGHT_BRACE } null\nLEFT_BRACE { null\nRIGHT_BRACE } null\nLEFT_BRACE { null\nEOF  null\n",
        },
        .{
            .name = "Braces",
            .input = "(}}{){(\n",
            .expected = "LEFT_PAREN ( null\nRIGHT_BRACE } null\nRIGHT_BRACE } null\nLEFT_BRACE { null\nRIGHT_PAREN ) null\nLEFT_BRACE { null\nLEFT_PAREN ( null\nEOF  null\n",
        },

        .{
            .name = "Other single-character tokens",
            .input = "+-\n",
            .expected = "PLUS + null\nMINUS - null\nEOF  null\n",
        },
        .{
            .name = "Other single-character tokens",
            .input = "++--**..,,;;\n",
            .expected = "PLUS + null\nPLUS + null\nMINUS - null\nMINUS - null\nSTAR * null\nSTAR * null\nDOT . null\nDOT . null\nCOMMA , null\nCOMMA , null\nSEMICOLON ; null\nSEMICOLON ; null\nEOF  null\n",
        },
        .{
            .name = "Other single-character tokens",
            .input = "-+;.+-;\n",
            .expected = "MINUS - null\nPLUS + null\nSEMICOLON ; null\nDOT . null\nPLUS + null\nMINUS - null\nSEMICOLON ; null\nEOF  null\n",
        },
        .{
            .name = "Other single-character tokens",
            .input = "({*.,+*})" ++ "\n",
            .expected = "LEFT_PAREN ( null\nLEFT_BRACE { null\nSTAR * null\nDOT . null\nCOMMA , null\nPLUS + null\nSTAR * null\nRIGHT_BRACE } null\nRIGHT_PAREN ) null\nEOF  null\n",
        },

        .{
            .name = "Assignment & equality Operators",
            .input = "=\n",
            .expected = "EQUAL = null\nEOF  null\n",
        },
        .{
            .name = "Assignment & equality Operators",
            .input = "==\n",
            .expected = "EQUAL_EQUAL == null\nEOF  null\n",
        },
        .{
            .name = "Assignment & equality Operators",
            .input = "={===}\n",
            .expected = "EQUAL = null\nLEFT_BRACE { null\nEQUAL_EQUAL == null\nEQUAL = null\nRIGHT_BRACE } null\nEOF  null\n",
        },
        .{
            .name = "Assignment & equality Operators",
            .input = "({=}){==}\n",
            .expected = "LEFT_PAREN ( null\nLEFT_BRACE { null\nEQUAL = null\nRIGHT_BRACE } null\nRIGHT_PAREN ) null\nLEFT_BRACE { null\nEQUAL_EQUAL == null\nRIGHT_BRACE } null\nEOF  null\n",
        },

        .{
            .name = "Negation & inequality operators",
            .input = "!=\n",
            .expected = "BANG_EQUAL != null\nEOF  null\n",
        },
        .{
            .name = "Negation & inequality operators",
            .input = "!!===\n",
            .expected = "BANG ! null\nBANG_EQUAL != null\nEQUAL_EQUAL == null\nEOF  null\n",
        },
        .{
            .name = "Negation & inequality operators",
            .input = "!{!}(!===)=\n",
            .expected = "BANG ! null\nLEFT_BRACE { null\nBANG ! null\nRIGHT_BRACE } null\nLEFT_PAREN ( null\nBANG_EQUAL != null\nEQUAL_EQUAL == null\nRIGHT_PAREN ) null\nEQUAL = null\nEOF  null\n",
        },

        .{
            .name = "Relational operators",
            .input = ">=\n",
            .expected = "GREATER_EQUAL >= null\nEOF  null\n",
        },
        .{
            .name = "Relational operators",
            .input = "<<<=>>>=\n",
            .expected = "LESS < null\nLESS < null\nLESS_EQUAL <= null\nGREATER > null\nGREATER > null\nGREATER_EQUAL >= null\nEOF  null\n",
        },
        .{
            .name = "Relational operators",
            .input = "<>>=>=<=\n",
            .expected = "LESS < null\nGREATER > null\nGREATER_EQUAL >= null\nGREATER_EQUAL >= null\nLESS_EQUAL <= null\nEOF  null\n",
        },
        .{
            .name = "Relational operators",
            .input = "(){><!}\n",
            .expected = "LEFT_PAREN ( null\nRIGHT_PAREN ) null\nLEFT_BRACE { null\nGREATER > null\nLESS < null\nBANG ! null\nRIGHT_BRACE } null\nEOF  null\n",
        },

        .{
            .name = "Division operator & comments",
            .input = "//Comment\n",
            .expected = "EOF  null\n",
        },
        .{
            .name = "Division operator & comments",
            .input = "() //Comment\n",
            .expected = "LEFT_PAREN ( null\nRIGHT_PAREN ) null\nEOF  null\n",
        },
        .{
            .name = "Division operator & comments",
            .input = "/()\n",
            .expected = "SLASH / null\nLEFT_PAREN ( null\nRIGHT_PAREN ) null\nEOF  null\n",
        },
        // [tester::#ER2] Running tests for Stage #ER2 (Scanning: Whitespace)
        //
        // (///Unicode:£§᯽☺♣)
        //LEFT_PAREN ( null
        //EOF  null
        //
        // /
        //SLASH / null
        //EOF  null
        //
        // ({(=-!)})//Comment
        //LEFT_PAREN ( null
        //LEFT_BRACE { null
        //LEFT_PAREN ( null
        //EQUAL = null
        //MINUS - null
        //BANG ! null
        //RIGHT_PAREN ) null
        //RIGHT_BRACE } null
        //RIGHT_PAREN ) null
        //EOF  null

        // [tester::#ER2] Running tests for Stage #ER2 (Scanning: Whitespace)

        // [tester::#ER2] [test-1] Running test case: 1
        // [tester::#ER2] [test-1] Writing contents to ./test.lox:
        // [tester::#ER2] [test-1] [test.lox] <|SPACE|>
        // [tester::#ER2] [test-1] $ ./your_program.sh tokenize test.lox
        // [your_program] EOF  null
        // [tester::#ER2] [test-1] ✓ 1 line(s) match on stdout
        // [tester::#ER2] [test-1] ✓ Received exit code 0.

        // [tester::#ER2] [test-2] Running test case: 2
        // [tester::#ER2] [test-2] Writing contents to ./test.lox:
        // [tester::#ER2] [test-2] [test.lox]  <|TAB|>
        // [tester::#ER2] [test-2] [test.lox] <|SPACE|>
        // [tester::#ER2] [test-2] $ ./your_program.sh tokenize test.lox
        // [your_program] EOF  null
        // [tester::#ER2] [test-2] ✓ 1 line(s) match on stdout
        // [tester::#ER2] [test-2] ✓ Received exit code 0.

        // [tester::#ER2] [test-3] Running test case: 3
        // [tester::#ER2] [test-3] Writing contents to ./test.lox:
        // [tester::#ER2] [test-3] [test.lox] {
        // [tester::#ER2] [test-3] [test.lox]  }
        // [tester::#ER2] [test-3] [test.lox] ((<|TAB|>; +-))
        // [tester::#ER2] [test-3] $ ./your_program.sh tokenize test.lox
        // [your_program] LEFT_BRACE { null
        // [your_program] RIGHT_BRACE } null
        // [your_program] LEFT_PAREN ( null
        // [your_program] LEFT_PAREN ( null
        // [your_program] SEMICOLON ; null
        // [your_program] PLUS + null
        // [your_program] MINUS - null
        // [your_program] RIGHT_PAREN ) null
        // [your_program] RIGHT_PAREN ) null
        // [your_program] EOF  null
        // [tester::#ER2] [test-3] ✓ 10 line(s) match on stdout
        // [tester::#ER2] [test-3] ✓ Received exit code 0.

        // [tester::#ER2] [test-4] Running test case: 4
        // [tester::#ER2] [test-4] Writing contents to ./test.lox:
        // [tester::#ER2] [test-4] [test.lox] {
        // [tester::#ER2] [test-4] [test.lox] <|TAB|><|TAB|>
        // [tester::#ER2] [test-4] [test.lox]  }
        // [tester::#ER2] [test-4] [test.lox] ((
        // [tester::#ER2] [test-4] [test.lox] <=>=,<|TAB|>))
        // [tester::#ER2] [test-4] $ ./your_program.sh tokenize test.lox
        // [your_program] LEFT_BRACE { null
        // [your_program] RIGHT_BRACE } null
        // [your_program] LEFT_PAREN ( null
        // [your_program] LEFT_PAREN ( null
        // [your_program] LESS_EQUAL <= null
        // [your_program] GREATER_EQUAL >= null
        // [your_program] COMMA , null
        // [your_program] RIGHT_PAREN ) null
        // [your_program] RIGHT_PAREN ) null
        // [your_program] EOF  null
        // [tester::#ER2] [test-4] ✓ 10 line(s) match on stdout
        // [tester::#ER2] [test-4] ✓ Received exit code 0.

        // <TOKEN_TYPE> <LEXEME> <LITERAL>
        .{
            .name = "String literals",
            .input = "\"hello\"\n",
            .expected = "STRING \"hello\" hello\nEOF  null\n",
        },
        .{
            .name = "String literals",
            .input = "\"foo baz\"\n",
            .expected = "STRING \"foo baz\" foo baz\nEOF  null\n",
        },
        .{
            .name = "String literals",
            .input = "\"foo \tbar 123 // hello world!\"", //"foo <|TAB|>bar 123 // hello world!"
            .expected = "STRING \"foo \tbar 123 // hello world!\" foo \tbar 123 // hello world!\nEOF  null\n",
        },
        .{
            .name = "String literals",
            .input = "(\"hello\"+\"foo\") != \"other_string\"",
            .expected = "LEFT_PAREN ( null\nSTRING \"hello\" hello\nPLUS + null\nSTRING \"foo\" foo\nRIGHT_PAREN ) null\nBANG_EQUAL != null\nSTRING \"other_string\" other_string\nEOF  null\n",
        },

        .{
            .name = "Number literals",
            .input = "63",
            .expected = "NUMBER 63 63.0\nEOF  null\n",
        },
        .{
            .name = "Number literals",
            .input = "42",
            .expected = "NUMBER 42 42.0\nEOF  null\n",
        },
        .{
            .name = "Number literals",
            .input =
            \\123
            \\123.456
            \\.456
            \\123.
            \\
            ,
            .expected =
            \\NUMBER 123 123.0
            \\NUMBER 123.456 123.456
            \\DOT . null
            \\NUMBER 456 456.0
            \\NUMBER 123 123.0
            \\DOT . null
            \\EOF  null
            \\
            ,
        },
        .{
            .name = "Number literals",
            .input =
            \\"quz" = "baz" != (22 == 90)
            \\
            ,
            .expected =
            \\STRING "quz" quz
            \\EQUAL = null
            \\STRING "baz" baz
            \\BANG_EQUAL != null
            \\LEFT_PAREN ( null
            \\NUMBER 22 22.0
            \\EQUAL_EQUAL == null
            \\NUMBER 90 90.0
            \\RIGHT_PAREN ) null
            \\EOF  null
            \\
            ,
        },
        .{
            .name = "Number literals",
            .input =
            \\(19+76) > 96 != ("Success" != "Failure") != (74 >= 88)
            \\
            ,
            .expected =
            \\LEFT_PAREN ( null
            \\NUMBER 19 19.0
            \\PLUS + null
            \\NUMBER 76 76.0
            \\RIGHT_PAREN ) null
            \\GREATER > null
            \\NUMBER 96 96.0
            \\BANG_EQUAL != null
            \\LEFT_PAREN ( null
            \\STRING "Success" Success
            \\BANG_EQUAL != null
            \\STRING "Failure" Failure
            \\RIGHT_PAREN ) null
            \\BANG_EQUAL != null
            \\LEFT_PAREN ( null
            \\NUMBER 74 74.0
            \\GREATER_EQUAL >= null
            \\NUMBER 88 88.0
            \\RIGHT_PAREN ) null
            \\EOF  null
            \\
            ,
        },

        // [See also](https://github.com/munificent/craftinginterpreters/blob/01e6f5b8f3e5dfa65674c2f9cf4700d73ab41cf8/test/scanning/identifiers.lox)
        .{
            .name = "Identifiers",
            .input =
            \\foo bar _hello
            \\_123world_ 6az bar baz f00
            \\andy formless fo _ _123 _abc ab123
            \\abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_
            \\
            ,
            .expected =
            \\IDENTIFIER foo null
            \\IDENTIFIER bar null
            \\IDENTIFIER _hello null
            \\IDENTIFIER _123world_ null
            \\NUMBER 6 6.0
            \\IDENTIFIER az null
            \\IDENTIFIER bar null
            \\IDENTIFIER baz null
            \\IDENTIFIER f00 null
            \\IDENTIFIER andy null
            \\IDENTIFIER formless null
            \\IDENTIFIER fo null
            \\IDENTIFIER _ null
            \\IDENTIFIER _123 null
            \\IDENTIFIER _abc null
            \\IDENTIFIER ab123 null
            \\IDENTIFIER abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_ null
            \\EOF  null
            \\
            ,
        },

        .{
            .name = "Identifiers",
            .input =
            \\{
            \\// This is a complex test case
            \\str1 = "Test"
            \\str2 = "Case"
            \\num1 = 100
            \\num2 = 200.00
            \\result = (str1 == str2) != ((num1 + num2) >= 300)
            \\}
            \\
            ,
            .expected =
            \\LEFT_BRACE { null
            \\IDENTIFIER str1 null
            \\EQUAL = null
            \\STRING "Test" Test
            \\IDENTIFIER str2 null
            \\EQUAL = null
            \\STRING "Case" Case
            \\IDENTIFIER num1 null
            \\EQUAL = null
            \\NUMBER 100 100.0
            \\IDENTIFIER num2 null
            \\EQUAL = null
            \\NUMBER 200.00 200.0
            \\IDENTIFIER result null
            \\EQUAL = null
            \\LEFT_PAREN ( null
            \\IDENTIFIER str1 null
            \\EQUAL_EQUAL == null
            \\IDENTIFIER str2 null
            \\RIGHT_PAREN ) null
            \\BANG_EQUAL != null
            \\LEFT_PAREN ( null
            \\LEFT_PAREN ( null
            \\IDENTIFIER num1 null
            \\PLUS + null
            \\IDENTIFIER num2 null
            \\RIGHT_PAREN ) null
            \\GREATER_EQUAL >= null
            \\NUMBER 300 300.0
            \\RIGHT_PAREN ) null
            \\RIGHT_BRACE } null
            \\EOF  null
            \\
            ,
        },

        //[See also](https://github.com/munificent/craftinginterpreters/blob/01e6f5b8f3e5dfa65674c2f9cf4700d73ab41cf8/test/scanning/keywords.lox)
        .{
            .name = "Reserved words", //keywords
            .input =
            \\false
            \\and class else false for fun if nil or return super this true var while
            \\
            ,
            .expected =
            \\FALSE false null
            \\AND and null
            \\CLASS class null
            \\ELSE else null
            \\FALSE false null
            \\FOR for null
            \\FUN fun null
            \\IF if null
            \\NIL nil null
            \\OR or null
            \\RETURN return null
            \\SUPER super null
            \\THIS this null
            \\TRUE true null
            \\VAR var null
            \\WHILE while null
            \\EOF  null
            \\
            ,
        },
        .{
            .name = "Reserved words", //keywords
            .input =
            \\FUN RETURN THIS CLASS SUPER false IF super FALSE NIL FOR return true print for this nil class or if fun while AND TRUE var PRINT VAR WHILE else OR ELSE and
            \\
            ,
            .expected =
            \\IDENTIFIER FUN null
            \\IDENTIFIER RETURN null
            \\IDENTIFIER THIS null
            \\IDENTIFIER CLASS null
            \\IDENTIFIER SUPER null
            \\FALSE false null
            \\IDENTIFIER IF null
            \\SUPER super null
            \\IDENTIFIER FALSE null
            \\IDENTIFIER NIL null
            \\IDENTIFIER FOR null
            \\RETURN return null
            \\TRUE true null
            \\PRINT print null
            \\FOR for null
            \\THIS this null
            \\NIL nil null
            \\CLASS class null
            \\OR or null
            \\IF if null
            \\FUN fun null
            \\WHILE while null
            \\IDENTIFIER AND null
            \\IDENTIFIER TRUE null
            \\VAR var null
            \\IDENTIFIER PRINT null
            \\IDENTIFIER VAR null
            \\IDENTIFIER WHILE null
            \\ELSE else null
            \\IDENTIFIER OR null
            \\IDENTIFIER ELSE null
            \\AND and null
            \\EOF  null
            \\
            ,
        },
        .{
            .name = "Reserved words",
            .input =
            \\var greeting = "Hello"
            \\if (greeting == "Hello") {
            \\    return true
            \\} else {
            \\    return false
            \\}
            \\
            \\var result = (a + b) > 7 or "Success" != "Failure" or x >= 5
            \\while (result) {
            \\    var counter = 0
            \\    counter = counter + 1
            \\    if (counter == 10) {
            \\        return nil
            \\    }
            \\}
            \\
            ,
            .expected =
            \\VAR var null
            \\IDENTIFIER greeting null
            \\EQUAL = null
            \\STRING "Hello" Hello
            \\IF if null
            \\LEFT_PAREN ( null
            \\IDENTIFIER greeting null
            \\EQUAL_EQUAL == null
            \\STRING "Hello" Hello
            \\RIGHT_PAREN ) null
            \\LEFT_BRACE { null
            \\RETURN return null
            \\TRUE true null
            \\RIGHT_BRACE } null
            \\ELSE else null
            \\LEFT_BRACE { null
            \\RETURN return null
            \\FALSE false null
            \\RIGHT_BRACE } null
            \\VAR var null
            \\IDENTIFIER result null
            \\EQUAL = null
            \\LEFT_PAREN ( null
            \\IDENTIFIER a null
            \\PLUS + null
            \\IDENTIFIER b null
            \\RIGHT_PAREN ) null
            \\GREATER > null
            \\NUMBER 7 7.0
            \\OR or null
            \\STRING "Success" Success
            \\BANG_EQUAL != null
            \\STRING "Failure" Failure
            \\OR or null
            \\IDENTIFIER x null
            \\GREATER_EQUAL >= null
            \\NUMBER 5 5.0
            \\WHILE while null
            \\LEFT_PAREN ( null
            \\IDENTIFIER result null
            \\RIGHT_PAREN ) null
            \\LEFT_BRACE { null
            \\VAR var null
            \\IDENTIFIER counter null
            \\EQUAL = null
            \\NUMBER 0 0.0
            \\IDENTIFIER counter null
            \\EQUAL = null
            \\IDENTIFIER counter null
            \\PLUS + null
            \\NUMBER 1 1.0
            \\IF if null
            \\LEFT_PAREN ( null
            \\IDENTIFIER counter null
            \\EQUAL_EQUAL == null
            \\NUMBER 10 10.0
            \\RIGHT_PAREN ) null
            \\LEFT_BRACE { null
            \\RETURN return null
            \\NIL nil null
            \\RIGHT_BRACE } null
            \\RIGHT_BRACE } null
            \\EOF  null
            \\
            ,
        },
    };

    for (test_cases) |tc| {
        var buf: [1024 * 2]u8 = undefined;
        var w = std.io.fixedBufferStream(&buf);
        const exit_code = try main.run(w.writer(), "tokenize", tc.input);

        const actual = w.getWritten();

        testing.expectEqualStrings(tc.expected, actual) catch |err| {
            std.log.err("{}\n\t{}\nActual:\n{s}\n", .{ err, tc, actual });
        };

        testing.expectEqual(0, exit_code) catch |err| {
            std.log.err("{}\nInput:\n{s}\n", .{ err, tc.input });
        };
    }
}

//
//
// Lexical Errors
//
//
//

const TestCaseLexicalErrors = struct {
    name: []const u8,
    input: []const u8,
    expected: u8 = @intFromEnum(root.ErrorCode.syntax_error),

    pub fn format(self: TestCaseLexicalErrors, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "Test Case: {any}\nInput:\n{any}\nExpected:\n{any}", .{
            self.name, self.input, self.expected,
        });
    }
};

test "Errors for Scanning -> function run -> command: tokenize" {
    const test_cases = [_]TestCaseLexicalErrors{
        .{ .name = "Lexical errors", .input = "@\n" },
        .{ .name = "Lexical errors", .input = ",.$(#\n" },
        .{ .name = "Lexical errors", .input = "%%@$#\n" },
        .{ .name = "Lexical errors", .input = "{(#%+.-$*)}\n" },

        .{ .name = "Assignment & equality Operators", .input = "((%$=@#))\n" },
        .{ .name = "Negation & inequality operators", .input = "{(%$!===#)}\n" },

        // [tester::#TZ7] Running tests for Stage #TZ7 (Scanning: Multi-line errors)
        // [tester::#TZ7] [test-1] Running test case: 1
        // [tester::#TZ7] [test-1] Writing contents to ./test.lox:
        // [tester::#TZ7] [test-1] [test.lox] ()<|SPACE|><|TAB|>@
        // [tester::#TZ7] [test-1] $ ./your_program.sh tokenize test.lox
        // [your_program] LEFT_PAREN ( null
        // [your_program] RIGHT_PAREN ) null
        // [your_program] EOF  null
        // [your_program] [line 2] Error: Unexpected character: @
        // [tester::#TZ7] [test-1] ✓ 1 line(s) match on stderr
        // [tester::#TZ7] [test-1] ✓ 3 line(s) match on stdout
        // [tester::#TZ7] [test-1] ✓ Received exit code 65.
        // [tester::#TZ7] [test-2] Running test case: 2
        // [tester::#TZ7] [test-2] Writing contents to ./test.lox:
        // [tester::#TZ7] [test-2] [test.lox] %#$@
        // [tester::#TZ7] [test-2] $ ./your_program.sh tokenize test.lox
        // [your_program] EOF  null
        // [your_program] [line 1] Error: Unexpected character: %
        // [your_program] [line 1] Error: Unexpected character: #
        // [your_program] [line 1] Error: Unexpected character: $
        // [your_program] [line 1] Error: Unexpected character: @
        // [tester::#TZ7] [test-2] ✓ 4 line(s) match on stderr
        // [tester::#TZ7] [test-2] ✓ 1 line(s) match on stdout
        // [tester::#TZ7] [test-2] ✓ Received exit code 65.
        // [tester::#TZ7] [test-3] Running test case: 3
        // [tester::#TZ7] [test-3] Writing contents to ./test.lox:
        // [tester::#TZ7] [test-3] [test.lox] ()  #<|TAB|>{}
        // [tester::#TZ7] [test-3] [test.lox] @
        // [tester::#TZ7] [test-3] [test.lox] $
        // [tester::#TZ7] [test-3] [test.lox] +++
        // [tester::#TZ7] [test-3] [test.lox] // Let's Go!
        // [tester::#TZ7] [test-3] [test.lox] +++
        // [tester::#TZ7] [test-3] [test.lox] #
        // [tester::#TZ7] [test-3] $ ./your_program.sh tokenize test.lox
        // [your_program] [line 1] Error: Unexpected character: #
        // [your_program] [line 2] Error: Unexpected character: @
        // [your_program] LEFT_PAREN ( null
        // [your_program] RIGHT_PAREN ) null
        // [your_program] [line 3] Error: Unexpected character: $
        // [your_program] [line 7] Error: Unexpected character: #
        // [your_program] LEFT_BRACE { null
        // [your_program] RIGHT_BRACE } null
        // [your_program] PLUS + null
        // [your_program] PLUS + null
        // [your_program] PLUS + null
        // [your_program] PLUS + null
        // [your_program] PLUS + null
        // [your_program] PLUS + null
        // [your_program] EOF  null
        // [tester::#TZ7] [test-3] ✓ 4 line(s) match on stderr
        // [tester::#TZ7] [test-3] ✓ 11 line(s) match on stdout
        // [tester::#TZ7] [test-3] ✓ Received exit code 65.
        // [tester::#TZ7] [test-4] Running test case: 4
        // [tester::#TZ7] [test-4] Writing contents to ./test.lox:
        // [tester::#TZ7] [test-4] [test.lox] ({. %})
        // [tester::#TZ7] [test-4] $ ./your_program.sh tokenize test.lox
        // [your_program] LEFT_PAREN ( null
        // [your_program] LEFT_BRACE { null
        // [your_program] DOT . null
        // [your_program] RIGHT_BRACE } null
        // [your_program] [line 1] Error: Unexpected character: %
        // [your_program] RIGHT_PAREN ) null
        // [your_program] EOF  null
        // [tester::#TZ7] [test-4] ✓ 1 line(s) match on stderr
        // [tester::#TZ7] [test-4] ✓ 6 line(s) match on stdout
        // [tester::#TZ7] [test-4] ✓ Received exit code 65.
        // [tester::#TZ7] Test passed.

        // remote: [tester::#UE7] [test-2] [test.lox] "baz" "unterminated
        // remote: [tester::#UE7] [test-2] $ ./your_program.sh tokenize test.lox
        // remote: [your_program] [line 1] Error: Unterminated string.
        // remote: [your_program] STRING "baz" baz
        // remote: [your_program] EOF  null
        // remote: [tester::#UE7] [test-2] ✓ 1 line(s) match on stderr
        // remote: [tester::#UE7] [test-2] ✓ 2 line(s) match on stdout
        // remote: [tester::#UE7] [test-2] ✓ Received exit code 65.
    };

    for (test_cases) |tc| {
        var buf: [1024]u8 = undefined;
        var w = std.io.fixedBufferStream(&buf);

        const actual = try main.run(w.writer(), "tokenize", tc.input);

        testing.expectEqual(tc.expected, actual) catch |err| {
            std.log.err("{}\n\t{}\nActual:\n{}\n", .{ err, tc, actual });
        };
    }
}
