# Notes

## Makefile

In a **Makefile**, functions are used to manipulate text, file names, and
command outputs. These functions provide powerful ways to automate tasks and
transform data. Functions in a Makefile are called with the following syntax:

```makefile
$(function_name arguments)
```

Here are some commonly used functions in a Makefile:

### 1. **`$(shell ...)`**

Executes a shell command and returns the output.

**Example:**

```makefile
CURRENT_DIR := $(shell pwd)
```

### 2. **`$(wildcard ...)`**

Returns the list of files that match the given pattern.

**Example:**

```makefile
SRC_FILES := $(wildcard src/*.c)
```

### 3. **`$(subst from, to, text)`**

Replaces all instances of `from` with `to` in `text`.

**Example:**

```makefile
NEW_STR := $(subst .c,.o,file.c)
```

### 4. **`$(patsubst pattern, replacement, text)`**

Replaces words matching a pattern in `text` with `replacement`.

**Example:**

```makefile
OBJECTS := $(patsubst %.c,%.o,$(wildcard *.c))
```

### 5. **`$(addprefix prefix, names)`**

Adds a prefix to each word in `names`.

**Example:**

```makefile
OBJ_FILES := $(addprefix obj/, file1.o file2.o)
```

### 6. **`$(addsuffix suffix, names)`**

Adds a suffix to each word in `names`.

**Example:**

```makefile
OBJ_FILES := $(addsuffix .o, file1 file2)
```

### 7. **`$(notdir ...)`**

Removes the directory part of each file name.

**Example:**

```makefile
FILES := $(notdir src/file.c)
```

### 8. **`$(dir ...)`**

Extracts the directory part of each file name.

**Example:**

```makefile
DIRS := $(dir src/file.c)
```

### 9. **`$(join list1, list2)`**

Concatenates two lists element-wise.

**Example:**

```makefile
FILES := $(join dir1/, file1 file2)
```

### 10. **`$(sort ...)`**

Sorts the words in a list and removes duplicates.

**Example:**

```makefile
SORTED := $(sort b a c a)
```

### 11. **`$(filter pattern, text)`**

Selects words in `text` that match the `pattern`.

**Example:**

```makefile
C_FILES := $(filter %.c, $(wildcard *))
```

### 12. **`$(filter-out pattern, text)`**

Selects words in `text` that **do not** match the `pattern`.

**Example:**

```makefile
NON_C_FILES := $(filter-out %.c, $(wildcard *))
```

### 13. **`$(if condition, then-part, else-part)`**

Conditional evaluation based on whether `condition` is non-empty.

**Example:**

```makefile
VAR := $(if $(wildcard src/*.c), found, not found)
```

### 14. **`$(findstring find, in)`**

Checks if `find` is in `in`. Returns the `find` string if found.

**Example:**

```makefile
FOUND := $(findstring main,main.c)
```

### 15. **`$(strip ...)`**

Removes leading and trailing whitespace.

**Example:**

```makefile
STRIPPED := $(strip $(VAR))
```

### 16. **`$(foreach var, list, text)`**

Loops over a list, assigning each element to `var`, and evaluates `text`.

**Example:**

```makefile
FILES := a.c b.c
OBJECTS := $(foreach file,$(FILES),$(file:.c=.o))
```

---

These functions allow for sophisticated transformations and operations in a
Makefile, facilitating the automation of tasks such as compilation, dependency
management, and file handling.
