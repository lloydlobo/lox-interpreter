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

TEST_FILE := test.lox
TRACE_FLAGS := -freference-trace
VALGRIND := valgrind --leak-check=full --show-leak-kinds=all -s --track-origins=yes
ZIG_FILES := $(call generate_sources)
ZIG_SRCS := $(shell find src -name '*.zig' -not -name 'test_*.zig')
ZIG_TEST_FILES := $(shell find src -name 'test_*.zig')

.PHONY: clean
clean:
	@date && echo $(UNAME_S)
	rm -rf zig-out .zig-cache vgcore.*
	echo $$?

# 	@$(foreach src,$(call generate_sources), \
# 		# echo "Running ast-check on $(src)"; \
# 		zig ast-check $(src); \
# 	)
ast-check: $(ZIG_FILES)
	@echo "Running ast-check in parallel on $(ZIG_FILES)"
	@echo $(ZIG_FILES) | tr ' ' '\n' | parallel -j $(shell nproc) --halt-on-error 1 'zig ast-check {};'


#	@$(foreach src,$(ZIG_SRCS), \
#		echo "Running zig test on $(src)"; \
#		zig ast-check $(src); \
#		zig test $(src) $(TRACE_FLAGS); \
#	)
.PHONY: test-zig-srcs
test-zig-srcs: $(ZIG_SRCS) ./.zig-cache/o
	@make ast-check
	@echo "Running zig tests in parallel on $(ZIG_SRCS)"
	@echo $(ZIG_SRCS) | tr ' ' '\n' | parallel -j $(shell nproc) --halt soon,fail=3% 'echo "Running zig test on {}"; zig ast-check {}; zig test {} $(TRACE_FLAGS) 2>&1'

.PHONY: test-zig-srcs
SUPPRESS_STDERR_FLAGS := 2>&1 | head
SUPPRESS_STDERR_FLAGS = 
# : echo $(ZIG_TEST_FILES) | tr ' ' '\n' | parallel -j $(shell nproc) --halt-on-error 1 'echo "Running zig test on {}"; zig ast-check {}; zig test {} $(TRACE_FLAGS);'
test-zig-test-files: $(ZIG_TEST_FILES)
	@make ast-check
	@echo "Running zig tests in parallel on $(ZIG_TEST_FILES)"
	echo $(ZIG_TEST_FILES) | tr ' ' '\n' | parallel -j $(shell nproc) 'echo "Running zig test on {}"; zig ast-check {}; zig test {} $(TRACE_FLAGS) $(SUPPRESS_STDERR_FLAGS);'

.PHONY: test
test: $(ZIG_SRCS)
	@date && echo $(UNAME_S)

	# capture both stdout and stderr
	zig test src/test_statements_and_state.zig $(TRACE_FLAGS) 2>&1 | head &
	zig test src/test_expressions_evaluate.zig $(TRACE_FLAGS) 2>&1 | head &
	zig test src/test_expressions_parse.zig    $(TRACE_FLAGS) 2>&1 | head &
	zig test src/test_scanning.zig             $(TRACE_FLAGS) 2>&1 | head &

	wait

#
# COMMANDS
#

.PHONY: tokenize parse evaluate run

tokenize:
	@make ast-check
	@zig build --summary all $(TRACE_FLAGS)
	$(EXE) tokenize test.lox $(TRACE_FLAGS) && echo

parse:
	@make ast-check
	@zig build --summary all $(TRACE_FLAGS)
	$(EXE) parse test.lox $(TRACE_FLAGS) && echo

evaluate:
	@make ast-check
	@zig build --summary all $(TRACE_FLAGS)
	$(EXE) evaluate test.lox $(TRACE_FLAGS) && echo

run:
	@make ast-check
	@zig build --summary all $(TRACE_FLAGS)
	$(EXE) run test.lox $(TRACE_FLAGS) && echo


.PHONY: valgrind-tokenize valgrind-parse valgrind-evaluate valgrind-run

# @for src in $(ZIG_FILES); do echo "Running ast-check on $$src"; zig ast-check $$src; done
pre-valgrind:
	make ast-check
	@echo "Building project from build.zig"
	zig build --summary all

valgrind-tokenize:
	make -j4 pre-valgrind
	$(VALGRIND) $(EXE) tokenize $(TEST_FILE) $(TRACE_FLAGS)

valgrind-parse:
	make -j4 pre-valgrind
	$(VALGRIND) $(EXE) parse $(TEST_FILE) $(TRACE_FLAGS)

valgrind-evaluate:
	make -j4 pre-valgrind
	$(VALGRIND) $(EXE) evaluate $(TEST_FILE) $(TRACE_FLAGS)

valgrind-run:
	make -j4 pre-valgrind
	$(VALGRIND) $(EXE) run $(TEST_FILE) $(TRACE_FLAGS)


#
# EXAMPLES
#

define generate_example_sources
	$(wildcard examples/*.lox)
endef # or use `$(shell find src/examples -name '*.lox')`

EXAMPLE_FILES := $(call generate_example_sources)

# @printf "Running command 'run' on examples\n└─\x1b[37m$(EXAMPLE_FILES)\x1b[0m\n"
# @$(foreach src,$(EXAMPLE_FILES), \
# 	printf "\x1b[37mInterpreting file\n└─ $(src)\x1b[0m\n"; \
# 	$(VALGRIND) $(EXE) run $(src) $(TRACE_FLAGS); \
# )
.PHONY: run-examples
run-examples: $(EXAMPLE_FILES)
	make -j4 pre-valgrind
	@printf "Running command 'run' on examples\n└─\x1b[37m$(EXAMPLE_FILES)\x1b[0m\n"
	: "NOTE: halt on error is disabled. e.g: " parallel -j $(shell nproc) --halt-on-error 1 'printf ...'
	@echo $(EXAMPLE_FILES) | tr ' ' '\n' | parallel -j $(shell nproc) 'printf "\x1b[37mInterpreting file\n└─ {} \x1b[0m\n"; $(VALGRIND) $(EXE) run {} $(TRACE_FLAGS);'


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
