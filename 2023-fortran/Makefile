# Disable the default rules
MAKEFLAGS += --no-builtin-rules

define \n


endef
PERCENT = %

# Project name
NAME := aoc

# Configuration settings
FC := gfortran
FFLAGS := -std=gnu -Ofast
RM := rm -f

DIRS := $(wildcard src/[0123456789][0123456789])
DAYS := $(patsubst src/%, %, $(DIRS))

# List of all source files
SRCS := $(wildcard src/[0123456789][0123456789]/*.[Ff]90)

# Targets for actually doing the test runs
RUNS := $(patsubst %,run_%,$(DAYS))

# Targets for actually doing the test runs
REAL_RUNS := $(patsubst %,run_%/,$(DAYS))

# Targets for automatic downloading input
INPUTS := $(patsubst %,src/%/input.txt,$(DAYS))
OUTPUTS := $(patsubst %,src/%/output.txt,$(DAYS))

TEST_CASES := $(wildcard src/[0123456789][0123456789]/input_*.txt)
TEST_RUNS := $(TEST_CASES)
TEST_RUNS := $(subst src/,,$(TEST_RUNS))
TEST_RUNS := $(subst /input_,/,$(TEST_RUNS))
TEST_RUNS := $(patsubst %.txt,run_%,$(TEST_RUNS))

ALL_RUNS := $(REAL_RUNS) $()

# Executable
EXES_f := $(filter %.f90,$(SRCS))
EXES_f := $(EXES_f:.f90=.exe)
EXES_F := $(filter %.F90,$(SRCS))
EXES_F := $(EXES_F:.F90=.exe)
EXES := $(SRCS:.f90=.exe)
EXES := $(EXES:.F90=.exe)


$(EXES_f): %.exe: %.f90
	$(FC) $(FFLAGS) -o $@ $<
$(EXES_F): %.exe: %.F90
	$(FC) $(FFLAGS) -o $@ $<

$(INPUTS): src/%/input.txt: token.txt
	./input_dl.sh -y 2023 -d $(subst src/,,$(@:/input.txt=)) -o $@ -S $<

$(OUTPUTS): src/%/output.txt: token.txt
	./output_dl.sh -y 2023 -d $(subst src/,,$(@:/output.txt=)) -o $@ -S $<


# Run all tests for day N: any run_XX/example cases, and the real run_XX/ case
.SECONDEXPANSION:
$(RUNS): run_%: $$(filter $$@/$$(PERCENT),$$(TEST_RUNS)) $$@/

# Run only real input test
.SECONDEXPANSION:
$(REAL_RUNS): run_%: $$(filter src/$$(*D)/$$(PERCENT),$$(EXES)) src/$$(*D)/input.txt src/$$(*D)/output.txt
	- $< < src/$(*D)/input.txt | diff -w -N - src/$(*D)/output.txt

# Run specific test case
.SECONDEXPANSION:
$(TEST_RUNS): run_%: $$(filter src/$$(*D)/$$(PERCENT),$$(EXES)) src/$$(*D)/input_$$(*F).txt src/$$(*D)/output_$$(*F).txt
	- $< < src/$(*D)/input_$(*F).txt | diff -N - src/$(*D)/output_$(*F).txt


X = $(filter run_01%,$(TEST_RUNS))

.PHONY: all clean list run $(RUNS) $(REAL_RUNS) $(TEST_RUNS)
all: $(EXES)
clean:
	$(RM) $(filter %.exe, $(EXES))
	$(RM) $(filter %/input.txt, $(INPUTS))
	$(RM) $(filter %/output.txt, $(OUTPUTS))
list:
	@echo "SRCS: $(SRCS)"
	@echo "RUNS: $(RUNS)"
	@echo "REAL_RUNS: $(REAL_RUNS)"
	@echo "TEST_RUNS: $(TEST_RUNS)"
	@echo "X : $(X)"
run: $(RUNS)