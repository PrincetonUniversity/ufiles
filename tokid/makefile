
# ============================================================
# Makefile for tokid
#
# UFILES 0.10.0
#
# Builds:
#   libtokid.a
#   libtokid.so   (when MAKE_SO=1)
#
# Supported compilers:
#   Intel oneAPI 2023 (ifort)
#   GNU Fortran 13.2.0 (gfortran)
#
# Compatible with top-level UFILES:
#   make -C tokid tokid
#
# ============================================================

# ------------------------------------------------------------
# Compiler-dependent configuration
# ------------------------------------------------------------

include $(COMPILER_FLAGS)

# ------------------------------------------------------------
# Library configuration
# ------------------------------------------------------------

NAME := tokid

OBJDIR := $(PREFIX)/obj/$(NAME)
LIBDIR := $(PREFIX)/lib
MODDIR := $(PREFIX)/mod

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so

# ------------------------------------------------------------
# Source and object files
# ------------------------------------------------------------

SRCF := $(wildcard *.f90)
OBJF := $(patsubst %.f90,$(OBJDIR)/%.o,$(SRCF))

# ------------------------------------------------------------
# Compiler flags
# ------------------------------------------------------------

FDEF := $(FDEFS)
FINC := $(FINCL)
FFLG := $(FFLAGS)

# Compile all objects with position-independent code so
# they can be used in static and shared libraries.
PICFLAGS := -fPIC

# ------------------------------------------------------------
# Compiler-specific Fortran module output
#
# Intel:
#   -module <directory>
#
# GNU:
#   -J<directory>
# ------------------------------------------------------------

FC_NAME := $(notdir $(firstword $(FC)))

ifneq ($(filter ifort ifx,$(FC_NAME)),)
    MODOUT := -module $(MODDIR)
else
    MODOUT := -J$(MODDIR)
endif

# ------------------------------------------------------------
# Archive utility
#
# Explicitly use ar rcs to avoid differences in AR
# definitions across compiler environments.
# ------------------------------------------------------------

AR_CMD := ar
AR_OPTS := rcs

# ------------------------------------------------------------
# Shared-library linker flags
# ------------------------------------------------------------

SOFLAGS := -shared
RPATH_FLAGS := -Wl,-rpath,'$$ORIGIN'

# ------------------------------------------------------------
# Targets
#
# Preserve the tokid target expected by top-level UFILES.
# ------------------------------------------------------------

.PHONY: all libs $(NAME) check clean realclean clobber depend

.DEFAULT_GOAL := all

all: $(NAME)

libs: $(NAME)

$(NAME): $(LIBAR) $(if $(filter 1 TRUE true YES yes,$(MAKE_SO)),$(LIBSO))

# ------------------------------------------------------------
# Create output directories
# ------------------------------------------------------------

check:
	@mkdir -p $(LIBDIR) $(OBJDIR) $(MODDIR)

$(LIBDIR) $(OBJDIR) $(MODDIR):
	@mkdir -p $@

# ------------------------------------------------------------
# Static library
# ------------------------------------------------------------

$(LIBAR): $(OBJF) | $(LIBDIR)
	@echo "=================================================="
	@echo "Building $(NAME) static library"
	@echo "=================================================="
	@rm -f $@
	$(AR_CMD) $(AR_OPTS) $@ $(OBJF)

# ------------------------------------------------------------
# Shared library
# ------------------------------------------------------------

$(LIBSO): $(OBJF) | $(LIBDIR)
	@echo "=================================================="
	@echo "Building $(NAME) shared library"
	@echo "=================================================="
	$(FC) $(SOFLAGS) $(LDFLAGS) \
	    $(RPATH_FLAGS) \
	    -o $@ $(OBJF)

# ------------------------------------------------------------
# Compile free-form Fortran .f90
#
# IMPORTANT:
#   -c prevents the compiler from attempting to link
#   an executable, which would require MAIN__.
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.f90 | $(OBJDIR) $(MODDIR)
	@echo "Compiling Fortran: $<"
	$(FC) $(FFLG) $(PICFLAGS) \
	    $(FDEF) $(FINC) \
	    $(MODOUT) \
	    -c $< -o $@

# ------------------------------------------------------------
# Include Fortran dependencies if available
# ------------------------------------------------------------

-include make.depend

# ------------------------------------------------------------
# Cleanup
# ------------------------------------------------------------

clean:
	@echo "Cleaning $(NAME) objects"
	@rm -f $(OBJDIR)/*.o

realclean: clean
	@echo "Removing $(NAME) libraries"
	@rm -f $(LIBAR) $(LIBSO)

clobber: realclean
	@echo "Removing $(NAME) module files"
	@rm -f $(MODDIR)/tokid*.mod

# ------------------------------------------------------------
# Generate Fortran dependencies
# ------------------------------------------------------------

depend:
	@if command -v makedepf90 >/dev/null 2>&1; then \
	    makedepf90 -b OBJDIR $(SRCF) > make.depend; \
	    sed -i 's|OBJDIR|$$(OBJDIR)|g' make.depend; \
	else \
	    echo "WARNING: makedepf90 is not available"; \
	fi

# ============================================================
# End of Makefile
# ============================================================
