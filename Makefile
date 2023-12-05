# Disable the default rules
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

define \n


endef

# Project name
NAME := aoc

# Configuration settings
FC := gfortran
FFLAGS := -std=gnu
RM := rm -f

# List of all source files
SRCS := src/01/treb.f90 \
		src/02/cubes.f90

# Executable
EXES := $(patsubst %.f90, %.exe, $(SRCS))

# Targets for actually doing the test runs
RUNS := $(patsubst %.f90, %, $(SRCS))

.PHONY: all clean run $(RUNS)
all: $(EXES)

# Compile fortran files into executable
$(EXES): %.exe: %.f90
	$(FC) $(FFLAGS) -o $@ $<

# Run on tests
$(RUNS): %: %.exe
	$(foreach input, $(wildcard $(dir $@)input*.txt), $< < $(input) | diff -w -N - $(subst input,output,$(input)) || true ${\n})

run: $(RUNS)


# Cleanup, filter to avoid removing source code by accident
clean:
	$(RM) $(filter %.exe, $(EXES))