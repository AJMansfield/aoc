# Disable the default rules
MAKEFLAGS += --no-builtin-rules

define \n


endef

# Project name
NAME := aoc

# Configuration settings
FC := gfortran
FFLAGS := -std=gnu -O3
RM := rm -f

# List of all source files
SRCS_f := $(sort $(wildcard src/*/*.f90))
SRCS_F := $(sort $(wildcard src/*/*.F90))
SRCS := $(sort $(SRCS_f) $(SRCS_F))

# Executable
EXES_f := $(patsubst %.f90, %.exe, $(SRCS_f))
EXES_F :=$(patsubst %.F90, %.exe, $(SRCS_F))
EXES := $(sort $(EXES_f) $(EXES_F))

# Targets for actually doing the test runs
RUNS := $(patsubst %.exe, %_run, $(EXES))

$(EXES_f): %.exe: %.f90
	$(FC) $(FFLAGS) -o $@ $<
$(EXES_F): %.exe: %.F90
	$(FC) $(FFLAGS) -o $@ $<

# Run on tests
$(RUNS): %_run: %.exe
	$(foreach input, $(wildcard $(dir $@)input*.txt), $< < $(input) | diff -w -N - $(subst input,output,$(input)) || true ${\n})


.PHONY: all clean list run $(RUNS)
all: $(EXES)
clean:
	$(RM) $(filter %.exe, $(EXES))
list:
	@echo "SRCS: $(SRCS)"
	@echo "SRCS_f: $(SRCS_f)"
	@echo "SRCS_F: $(SRCS_F)"
	@echo "EXES: $(EXES)"
	@echo "EXES_f: $(EXES_f)"
	@echo "EXES_F: $(EXES_F)"
	@echo "RUNS: $(RUNS)"
run: $(RUNS)