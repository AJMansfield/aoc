# Disable the default rules
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

# Project name
NAME := aoc

# Configuration settings
FC := gfortran -std=gnu
RM := rm -f

# List of all source files
SRCS := src/01/treb.f90 \
		src/02/cubes.f90

# Create lists of the build artefacts in this project
EXES := $(patsubst %.f90, %.exe, $(SRCS))

# Declare all public targets
.PHONY: all clean
all: $(EXES)

# Create object files from Fortran source
$(EXES): %.exe: %.f90
	$(FC) -o $@ $<


# Cleanup, filter to avoid removing source code by accident
clean:
	$(RM) $(filter %.exe, $(EXES))