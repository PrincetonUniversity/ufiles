
# ============================================================
# Makefile: mdstransp
#
# UFILES 0.10.0
#
# Builds:
#   libmdstransp.a
#   libmdstransp.so  (MAKE_SO=1)
#
# Supported compilers:
#   Intel oneAPI 2023 (ifort / icx)
#   GCC / GNU Fortran 13.x
#
# IMPORTANT:
#   mdstransp is always compiled without optimization.
#
# Direct dependencies:
#   libufport
#   MDSplus
# ============================================================

include $(COMPILER_FLAGS)

# ------------------------------------------------------------
# Library configuration
# ------------------------------------------------------------

NAME := mdstransp

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so

MAKE_SO ?= 0

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------

SRCF90 := $(wildcard *.f90)
SRCFOR := $(wildcard *.for)
SRCC   := $(wildcard *.c)

SRCF := $(SRCF90) $(SRCFOR)

OBJF90 := $(patsubst %.f90,$(OBJDIR)/%.o,$(SRCF90))
OBJFOR := $(patsubst %.for,$(OBJDIR)/%.o,$(SRCFOR))
OBJC   := $(patsubst %.c,$(OBJDIR)/%.o,$(SRCC))

OBJF := $(OBJF90) $(OBJFOR)

OBJECTS := $(OBJF) $(OBJC)

# ------------------------------------------------------------
# Position-independent code
#
# Always compile with -fPIC so the same objects can be
# used in both static and shared libraries.
# ------------------------------------------------------------

PICFLAGS := -fPIC

# ------------------------------------------------------------
# Fortran flags
#
# Always use debug / non-optimized flags.
#
# The compilation recipes explicitly specify -c.
# ------------------------------------------------------------

FDEF := $(FDEFS)

FINC := $(FINCL) \
        -I$(MODDIR)

FFLG := $(FFLAGS_DBG) $(PICFLAGS)

# ------------------------------------------------------------
# C flags
#
# IMPORTANT:
#   Do not include $(CPP).
#
# In the Intel configuration:
#   CPP = -E
#
# Using -E produces preprocessed C source rather than
# a compiled ELF object.
# ------------------------------------------------------------

CDEF := $(CDEFS)

CINC := $(CINCL) \
        -I$(UFILES_HOME)/include

ifdef MDSPLUS_DIR
CINC += -I$(MDSPLUS_DIR)/include
endif

CFLG := $(CFLAGS_DBG) $(PICFLAGS)

# ------------------------------------------------------------
# Fortran module output
#
# Use the compiler configuration's MFLAG:
#
#   Intel: -module
#   GNU:   -J
# ------------------------------------------------------------

MODOUT := $(MFLAG) $(MODDIR)

# ------------------------------------------------------------
# Direct UFILES dependencies
# ------------------------------------------------------------

L_UFILES_DEP := \
    -L$(LIBDIR) \
    -lufport

# ------------------------------------------------------------
# Runtime library paths
# ------------------------------------------------------------

SHARED_RPATH := -Wl,-rpath,'$$ORIGIN'

# ------------------------------------------------------------
# Build configuration
# ------------------------------------------------------------

BUILD_SHARED := $(filter 1 TRUE true YES yes,$(MAKE_SO))

# ------------------------------------------------------------
# Targets
#
# Preserve the mdstransp target required by top-level
# UFILES.
# ------------------------------------------------------------

.PHONY: all libs $(NAME) check clean realclean clobber depend

.DEFAULT_GOAL := all

all: $(NAME)

libs: $(NAME)

$(NAME): $(LIBAR) $(if $(BUILD_SHARED),$(LIBSO))
	@echo "$(NAME) built without optimization"

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

$(LIBAR): $(OBJECTS) | $(LIBDIR)
	@echo "=================================================="
	@echo "Building $(NAME) static library"
	@echo "=================================================="
	@rm -f $@
	$(AR) $@ $(OBJECTS)

# ------------------------------------------------------------
# Shared library
#
# Library order:
#   mdstransp objects
#   libufport
#   MDSplus
# ------------------------------------------------------------

$(LIBSO): $(OBJECTS) | $(LIBDIR)
	@echo "=================================================="
	@echo "Building $(NAME) shared library"
	@echo "=================================================="
	@rm -f $@
	$(FC) -shared $(LDFLAGS) \
	    -o $@ \
	    $(OBJECTS) \
	    $(L_UFILES_DEP) \
	    $(L_MDSPLUS) \
	    $(SHARED_RPATH)

# ------------------------------------------------------------
# Fortran compilation: .for
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.for | $(OBJDIR) $(MODDIR)
	@echo "Compiling Fortran: $<"
	$(FC) $(FPP) $(FFLG) \
	    $(FDEF) $(FINC) \
	    $(MODOUT) \
	    -c $< -o $@

# ------------------------------------------------------------
# Fortran compilation: .f90
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.f90 | $(OBJDIR) $(MODDIR)
	@echo "Compiling Fortran: $<"
	$(FC) $(FPP) $(FFLG) \
	    $(FDEF) $(FINC) \
	    $(MODOUT) \
	    -c $< -o $@

# ------------------------------------------------------------
# C compilation
#
# CRITICAL FIX:
#   Removed $(CPP) to prevent preprocessing-only output.
#
# Correct:
#   icx -O0 -fPIC -c source.c -o source.o
#
# Incorrect:
#   icx -E -O0 -fPIC -c source.c -o source.o
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.c | $(OBJDIR)
	@echo "Compiling C: $<"
	$(CC) $(CFLG) \
	    $(CDEF) $(CINC) \
	    -c $< -o $@

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------

-include make.depend

# ------------------------------------------------------------
# Clean
# ------------------------------------------------------------

clean:
	@echo "Cleaning $(NAME) objects"
	@rm -f $(OBJDIR)/*.o

realclean: clean
	@echo "Removing $(NAME) libraries"
	@rm -f $(LIBAR) $(LIBSO)

clobber: realclean
	@echo "Removing $(NAME) module files"
	@rm -f $(MODDIR)/mdstransp*.mod

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
# End of mdstransp Makefile
# ============================================================
