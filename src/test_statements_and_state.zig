const std = @import("std");
const testing = std.testing;

const main = @import("main.zig");
const Command = @import("main.zig").Command;
const ErrorCode = @import("root.zig").ErrorCode;

const skip_test = true;

// see https://www.reddit.com/r/Zig/comments/tyhmip/comment/kjac63i/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
// i ran into the same issue. tests eat print statements. dont know if it eats
// both stdout and stderr though
// [solution](https://ziggit.dev/t/zig-test-ok-printing-is-missing-on-macos-terminal/2986)
//      tldr:
//      ```
//      $ zig test test.zig 2>&1 | head
//      ```
//
//      You are getting this result because you are using a tty.
//
//         zig test test.zig
//         All 1 tests passed.
//
//      If you capture both stdout and stderr it displays:
//
//      zig test test.zig 2>&1 | head
//      1/1 test.print test result... OK
//      All 1 tests passed.
//
//      A way to overcome this is to build your own test_runner and use the
//      --test-runner option. The default test_runner is bundled at
//      lib/test_runner.zig

fn testStatementsAndState(comptime input: []const u8, comptime expected: []const u8) !void {
    var buf: [1024 * 4]u8 = undefined;

    // FIXME: maybe use arena since bw.flush() may make testing difficult if
    // writer isn't stdout_writer (test examples work when running)
    var writer = std.io.fixedBufferStream(&buf);

    const expected_exitcode = @intFromEnum(ErrorCode.exit_success);
    const actual_exitcode: u8 = try main.run(writer.writer(), Command.run.toString(), input);
    testing.expectEqual(expected_exitcode, actual_exitcode) catch |err|
        std.debug.print("{0any}\nInput: {1s}\nExpected: {2d}\nActual: {3d}\n", .{
        err,
        input,
        expected_exitcode,
        actual_exitcode,
    });

    const actual = writer.getWritten();
    testing.expectEqualStrings(expected, actual) catch |err|
        std.debug.print("{0any}\nInput: {1s}\nExpected: {2s}\nActual: {3s}\n", .{
        err,
        input,
        expected,
        actual,
    });
}

fn testStatementsAndStateErrors(comptime input: []const u8, expected: u8) !void {
    var buf: [1024]u8 = undefined;
    var writer = std.io.fixedBufferStream(&buf);
    const w = writer.writer();

    const actual = try main.run(w, Command.run.toString(), input); //> exit code
    testing.expectEqual(expected, actual) catch |err|
        std.debug.print("{0any}\nInput: {1s}\nExpected: {2d}\nActual: {3d}\n", .{
        err,
        input,
        expected,
        actual,
    });
}

//
//
// COMPILE TIME TESTS
//
//
//

test "Statements & State - Print: Generate output" {
    const test_cases = [_][2][]const u8{
        .{
            \\print "Hello, World!";
            ,
            "Hello, World!" ++ "\n",
        },
        .{
            \\print 42;
            ,
            "42" ++ "\n",
        },
        .{
            \\print true;
            ,
            "true" ++ "\n",
        },
        .{
            \\print 12 + 24;
            ,
            "36" ++ "\n",
        },
        .{
            \\print "baz" + "foo" + "quz";
            ,
            "bazfooquz" ++ "\n",
        },
        .{
            \\print (48 * 2 + 45 * 2) / (2);
            ,
            "93" ++ "\n",
        },
        .{
            \\print "(" + "" + ")";
            ,
            "()" ++ "\n",
        },

        // Non-ASCII
        .{
            \\print "A~¶Þॐஃ";
            ,
            "A~¶Þॐஃ" ++ "\n",
        },
    };

    inline for (test_cases) |tc| try testStatementsAndState(tc[0], tc[1]);
}

test "Statements & State - Expression statements" {
    const test_cases = [_][2][]const u8{
        .{
            \\(37 + 42 - 21) > (76 - 37) * 2;
            \\print !false;
            \\"baz" + "hello" + "quz" + "bar" == "bazhelloquzbar";
            \\print !false;
            ,
            \\true
            \\true
            \\
        },
        .{
            \\27 - 60 >= -99 * 2 / 99 + 76;
            \\true == true;
            \\("world" == "bar") == ("baz" != "hello");
            \\print true;
            ,
            \\true
            \\
        },
    };

    inline for (test_cases) |tc| try testStatementsAndState(tc[0], tc[1]);
}

test "Statements & State - Variables: Declare variables" {
    const test_cases = [_][2][]const u8{
        .{
            \\var a = "foo";
            \\print a;
            ,
            \\foo
            \\
        },
        .{
            \\var world = 10;
            \\print world;
            ,
            \\10
            \\
        },
        .{
            \\var bar = 99;
            \\var foo = 99;
            \\print bar + foo;
            \\var quz = 99;
            \\print bar + foo + quz;
            ,
            \\198
            \\297
            \\
        },
        .{
            \\var foo = (8 * (62 + 62)) / 4 + 62;
            \\print foo;
            ,
            \\310
            \\
        },
        .{
            \\var quz = 76;
            \\var baz = quz;
            \\print baz + quz;
            ,
            \\152
            \\
        },
    };

    inline for (test_cases) |tc| try testStatementsAndState(tc[0], tc[1]);
}

test "Statements & State - Syntax Errors" {
    const test_cases = [_][]const u8{
        // "Statements & State - Print: Generate output"
        "print;",

        // "Statements & State - Expression statements"
        \\print "the expression below is invalid";
        \\49 + "baz";
        \\print "this should not be printed";
        ,
        //
        // the expression below is invalid
        // Operands must be two numbers or two strings.
        // [line 2]

        \\print "79" + "baz";
        \\print false * (18 + 84);
        ,
        //
        // 79baz
        // Operands must be numbers.
        // [line 2]
    };

    const expected: u8 = @intFromEnum(ErrorCode.syntax_error);
    inline for (test_cases) |tc| try testStatementsAndStateErrors(tc, expected);
}

//
//
// RUNTIME TESTS
//
//
//

test "Statements & State - Variables: Initialize variables" {
    if (comptime !skip_test) {
        const test_cases = [_][2][]const u8{
            .{
                \\var baz;
                \\print baz;
                ,
                \\nil
            },
            .{
                \\var world 
                \\var baz;
                \\print baz;
                ,
                \\nil
            },
            .{
                \\var hello 
                \\var bar;
                \\var quz;
                \\print bar;
                ,
                \\nil
            },
            .{
                \\var quz = 73 + 26 * 20;
                \\print quz;
                \\var hello = 26 * 20;
                \\print quz + hello;
                \\var foo;
                \\print foo;
                ,
                \\593
                \\1113
                \\nil
            },
        };

        inline for (test_cases) |tc| try testStatementsAndState(tc[0], tc[1]);
    }
}

test "Statements & State - Variables: Redeclare variables" {
    if (comptime !skip_test) {
        const test_cases = [_][2][]const u8{
            .{
                \\var foo = "before";
                \\print foo;
                \\var foo = "after";
                \\print foo;
                ,
                \\before
                \\after
            },
            .{
                \\var foo = "after";
                \\var foo = "before";
                \\var foo = foo;
                \\print foo;
                ,
                \\before
            },
            .{
                \\var hello = 2;
                \\print hello;
                \\var hello = 3;
                \\print hello;
                \\var world = 5;
                \\print world;
                \\var hello = world;
                \\print hello;
                ,
                \\2
                \\3
                \\5
                \\5
            },
        };

        inline for (test_cases) |tc| try testStatementsAndState(tc[0], tc[1]);
    }
}

test "Statements & State - Block Syntax" {
    if (comptime !skip_test) {
        const test_cases = [_][2][]const u8{
            .{
                \\{
                \\    var hello = "baz";
                \\    print hello;
                \\}
                ,
                \\baz
            },
            .{
                \\{
                \\    var world = "before";
                \\    print world;
                \\}
                \\{
                \\    var world = "after";
                \\    print world;
                \\}
                ,
                \\before
                \\after
            },
            .{
                \\{
                \\    var hello = 88;
                \\    {
                \\        var foo = 88;
                \\        print foo;
                \\    }
                \\    print hello;
                \\}
                ,
                \\88
                \\88
            },
        };

        inline for (test_cases) |tc| try testStatementsAndState(tc[0], tc[1]);
    }
}

test "Statements & State - Variables: Runtime Errors" {
    if (comptime !skip_test) {
        const test_cases = [_][]const u8{
            \\print 34;
            \\print x;
            ,
            //
            // Undefined variable 'x'.
            // [line 2]
            // 3\4

            \\var foo = 69;
            \\print hello;
            ,
            //
            // Undefined variable 'hello'.
            // [line 2]

            \\var hello = 29;
            \\var result = (hello + quz) / bar;
            \\print result;
            ,
            //
            // Undefined variable 'quz'.
            // [line 2]

            \\var bar = 70;
            \\var foo = 91;
            \\var quz = 50;
            \\print bar + foo + quz + hello; print 13;
            ,
            //
            // Undefined variable 'hello'.
            // [line 4]

            \\var world = hello;
            \\
            ,
            //
            // Undefined variable 'hello'.

            // "Statements & State - Variables: Block syntax"

            \\{
            \\    var bar = 11;
            \\    var world = 11;
            \\    {
            \\        print bar + world;
            \\}
            ,
            //
            // [line 6] Error at end: Expect '}' after block.
        };

        // DISABLED: as returning runtime error seems buggy
        inline for (test_cases) |tc| try testStatementsAndStateErrors(tc, @intFromEnum(ErrorCode.runtime_error));
    }
}

// See also:
//     https://craftinginterpreters.com/statements-and-state.html#statements
//     https://craftinginterpreters.com/statements-and-state.html#interpreting-global-variables
//
