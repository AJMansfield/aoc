# Disable the default rules
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

# Project name
NAME := aoc

# Configuration settings
FC := gfortran
AR := ar rcs
LD := $(FC)
RM := rm -f

# List of all source files
SRCS := 
TEST_SRCS := src/01/treb.f90

# Create lists of the build artefacts in this project
OBJS := $(addsuffix .o, $(SRCS))
TEST_OBJS := $(addsuffix .o, $(TEST_SRCS))
LIB := $(patsubst %, lib%.a, $(NAME))
TEST_EXE := $(patsubst %.f90, %.exe, $(TEST_SRCS))

# Declare all public targets
.PHONY: all clean
all: $(LIB) $(TEST_EXE)

# Create the static library from the object files
$(LIB): $(OBJS)
	$(AR) $@ $^

# Link the test executables
$(TEST_EXE): %.exe: %.f90.o $(LIB)
	$(LD) -o $@ $^

# Create object files from Fortran source
$(OBJS) $(TEST_OBJS): %.o: %
	$(FC) -c -o $@ $<


# Cleanup, filter to avoid removing source code by accident
clean:
	$(RM) $(filter %.o, $(OBJS) $(TEST_OBJS)) $(filter %.exe, $(TEST_EXE)) $(LIB) $(wildcard *.mod)