ifeq ($(OS),Windows_NT)
	UNAME_S := Windows
	EXE := .\\zig-out\\bin\\main.exe
else
	UNAME_S := $(shell uname -s)
	EXE := ./zig-out/bin/main
endif

define generate_sources
	$(wildcard src/*.zig)
endef # or use `$(shell find src -name '*.zig')`

SRCS := $(call generate_sources)
TRACE_FLAGS :=  -freference-trace
VALGRIND := valgrind --leak-check=full --show-leak-kinds=all -s --track-origins=yes

.PHONY: clean
clean:
	@date && echo $(UNAME_S)
	rm -rf zig-out .zig-cache vgcore.*
	echo $$?

.PHONY: ast-check
ast-check:
	@printf "Running ast-check\n└─\x1b[37m$(SRCS)\x1b[0m\n"
	@$(foreach src,$(call generate_sources), \
		# echo "Running ast-check on $(src)"; \
		zig ast-check $(src); \
	)

.PHONY: test
test:
	@date && echo $(UNAME_S)

	# capture both stdout and stderr
	zig test src/test_statements_and_state.zig $(TRACE_FLAGS) 2>&1 | head &
	zig test src/test_expressions_evaluate.zig $(TRACE_FLAGS) 2>&1 | head &
	zig test src/test_expressions_parse.zig    $(TRACE_FLAGS) 2>&1 | head &
	zig test src/test_scanning.zig             $(TRACE_FLAGS) 2>&1 | head &
	wait

.PHONY: watch-test
watch-test:
	@echo "Watching for changes..."
	find src -name '*.zig' | entr -cr make -j4 test

.PHONY: build-run
build-run:
	@date && echo $(UNAME_S)

	make ast-check
	zig build run --summary all

#
# COMMANDS
#

.PHONY: tokenize parse evaluate run

tokenize:
	@make ast-check
	@zig build --summary all
	@$(EXE) tokenize test.lox && echo

parse:
	@make ast-check
	@zig build --summary all
	@$(EXE) parse test.lox && echo

evaluate:
	@make ast-check
	@zig build --summary all
	@$(EXE) evaluate test.lox && echo

run:
	@make ast-check
	@zig build --summary all
	@$(EXE) run test.lox && echo


.PHONY: valgrind-tokenize valgrind-parse valgrind-evaluate valgrind-run

# @for src in $(SRCS); do echo "Running ast-check on $$src"; zig ast-check $$src; done
pre-valgrind:
	make ast-check
	@echo "Building project from build.zig"
	zig build --summary all

valgrind-tokenize:
	make -j4 pre-valgrind
	$(VALGRIND) $(EXE) tokenize test.lox $(TRACE_FLAGS)

valgrind-parse:
	make -j4 pre-valgrind
	$(VALGRIND) $(EXE) parse test.lox $(TRACE_FLAGS)

valgrind-evaluate:
	make -j4 pre-valgrind
	$(VALGRIND) $(EXE) evaluate test.lox $(TRACE_FLAGS)

valgrind-run:
	make -j4 pre-valgrind
	$(VALGRIND) $(EXE) run test.lox $(TRACE_FLAGS)


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
