EXE := ./zig-out/bin/main

TEST_FLAGS :=  -freference-trace
# TEST_FLAGS = 

ifeq ($(OS),Windows_NT)
	UNAME_S := Windows
else
	UNAME_S := $(shell uname -s)
endif

.PHONY: clean
clean:
	@date && echo $(UNAME_S)
	rm -rf zig-out zig-cache

.PHONY: test
test:
	@date && echo $(UNAME_S)

	# capture both stdout and stderr
	zig test src/test_statements_and_state.zig $(TEST_FLAGS) 2>&1 | head &
	zig test src/test_expressions_evaluate.zig $(TEST_FLAGS) 2>&1 | head &
	zig test src/test_expressions_parse.zig    $(TEST_FLAGS) 2>&1 | head &
	zig test src/test_scanning.zig             $(TEST_FLAGS) 2>&1 | head &

	wait

.PHONY: watch-test
watch-test:
	@echo "Watching for changes..."
	find src -name '*.zig' | entr -cr make -j4 test

.PHONY: build-run
build-run:
	@date && echo $(UNAME_S)
	zig ast-check src/main.zig
	zig build run --summary all

#
# COMMANDS
#

.PHONY: tokenize parse evaluate run

tokenize:
	@zig build
	@$(EXE) tokenize test.lox && echo

parse:
	@zig build
	@$(EXE) parse test.lox && echo

evaluate:
	@zig build
	@$(EXE) evaluate test.lox && echo

run:
	zig ast-check src/main.zig
	@zig build
	@$(EXE) run test.lox && echo


.PHONY: valgrind-tokenize valgrind-parse valgrind-evaluate valgrind-run

valgrind-tokenize:
	zig ast-check src/main.zig
	zig build
	valgrind --leak-check=full --show-leak-kinds=all -s --track-origins=yes $(EXE) tokenize test.lox

valgrind-parse:
	zig build
	valgrind --leak-check=full --show-leak-kinds=all -s --track-origins=yes $(EXE) parse test.lox

valgrind-evaluate:
	zig build
	valgrind --leak-check=full --show-leak-kinds=all -s --track-origins=yes $(EXE) evaluate test.lox

valgrind-run:
	zig build
	valgrind --leak-check=full --show-leak-kinds=all -s --track-origins=yes $(EXE) run test.lox


.PHONY: all

# // Usage:
# //
# //     $ ./your_program.sh tokenize test.lox
# //     $ ./zig-out/bin/main tokenize test.lox
# //     $ zig test src/main.zig
# //     $ find . -name '*.zig' | entr -crs 'date; zig test src/main.zig; echo exit status $?'
# //     $ find . -name '*.lox' | entr -crs 'date; make parse;    echo exit code: $?'
# //     $ find . -name '*.lox' | entr -crs 'date; make tokenize; echo exit code: $?'
# //     $ find . -name '*.lox' | entr -crs 'date; make evaluate; echo exit code: $?'
# //
