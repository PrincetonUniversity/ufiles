
# ============================================================
# Makefile for mds_dummy
#
# UFILES 0.10.0
#
# Builds:
#   libmds_dummy.a
#   libmds_dummy.so   (when MAKE_SO=1)
#
# Supported compilers:
#   GNU GCC / GFortran 13.2.0
#   Intel oneAPI 2023 (ifort / icx)
#
# Compatible with the top-level UFILES target:
#   make mds_dummy
#
# Required:
#   PREFIX
#   COMPILER_FLAGS
#
# Optional:
#   MAKE_SO=1
#   DEBUG=0/1
# ============================================================


# ------------------------------------------------------------
# Compiler-dependent configuration
# ------------------------------------------------------------

include $(COMPILER_FLAGS)


# ------------------------------------------------------------
# Library configuration
# ------------------------------------------------------------

NAME := mds_dummy

OBJDIR := $(PREFIX)/obj/$(NAME)
LIBDIR := $(PREFIX)/lib
MODDIR := $(PREFIX)/mod

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ------------------------------------------------------------
# Build options
# ------------------------------------------------------------

MAKE_SO ?= 0
DEBUG   ?= 0

# Compile position-independent objects for both static
# and shared libraries.
PICFLAGS := -fPIC


# ------------------------------------------------------------
# Source files
# ------------------------------------------------------------

SRCFOR := $(wildcard *.for)
SRCF   := $(wildcard *.f)
SRCF90 := $(wildcard *.f90)
SRCC   := $(wildcard *.c)

OBJFOR := $(patsubst %.for,$(OBJDIR)/%.o,$(SRCFOR))
OBJF   := $(patsubst %.f,$(OBJDIR)/%.o,$(SRCF))
OBJF90 := $(patsubst %.f90,$(OBJDIR)/%.o,$(SRCF90))
OBJC   := $(patsubst %.c,$(OBJDIR)/%.o,$(SRCC))

OBJECTS := $(OBJFOR) $(OBJF) $(OBJF90) $(OBJC)


# ------------------------------------------------------------
# Compiler flags
# ------------------------------------------------------------

FDEF := $(FDEFS)
FINC := $(FINCL)
FFLG := $(FFLAGS)

CDEF := $(CDEFS)
CINC := $(CINCL)
CFLG := $(CFLAGS)


# ------------------------------------------------------------
# Detect Intel / GNU Fortran
#
# Intel:
#   -module directory
#
# GNU:
#   -Jdirectory
# ------------------------------------------------------------

FC_NAME := $(notdir $(firstword $(FC)))

ifneq ($(filter ifort ifx,$(FC_NAME)),)
    MODOUT := -module $(MODDIR)
else
    MODOUT := -J$(MODDIR)
endif


# ------------------------------------------------------------
# Archive tools
#
# Use explicit ar rcs.
#
# Do not rely on AR from the compiler configuration
# because some environments define AR="ar rcs".
# ------------------------------------------------------------

AR_CMD := ar
AR_OPTS := rcs


# ------------------------------------------------------------
# Shared library flags
# ------------------------------------------------------------

SOFLAGS := -shared

RPATH_FLAGS := -Wl,-rpath,'$$ORIGIN'


# ------------------------------------------------------------
# Targets
#
# The top-level UFILES Makefile expects:
#
#   make -C mds_dummy mds_dummy
#
# Therefore $(NAME) must exist as a target.
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

$(LIBAR): $(OBJECTS) | $(LIBDIR)
	@echo "=================================================="
	@echo "Building $(NAME) static library"
	@echo "=================================================="
	@rm -f $@
	$(AR_CMD) $(AR_OPTS) $@ $(OBJECTS)


# ------------------------------------------------------------
# Shared library
# ------------------------------------------------------------

$(LIBSO): $(OBJECTS) | $(LIBDIR)
	@echo "=================================================="
	@echo "Building $(NAME) shared library"
	@echo "=================================================="
	$(FC) $(SOFLAGS) $(LDFLAGS) \
	    $(RPATH_FLAGS) \
	    -o $@ $(OBJECTS)


# ------------------------------------------------------------
# Compile fixed-form Fortran .for
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.for | $(OBJDIR) $(MODDIR)
	@echo "Compiling Fortran: $<"
	$(FC) $(FFLG) $(PICFLAGS) \
	    $(FDEF) $(FINC) \
	    $(MODOUT) \
	    -c $< -o $@


# ------------------------------------------------------------
# Compile fixed-form Fortran .f
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.f | $(OBJDIR) $(MODDIR)
	@echo "Compiling Fortran: $<"
	$(FC) $(FFLG) $(PICFLAGS) \
	    $(FDEF) $(FINC) \
	    $(MODOUT) \
	    -c $< -o $@


# ------------------------------------------------------------
# Compile free-form Fortran .f90
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.f90 | $(OBJDIR) $(MODDIR)
	@echo "Compiling Fortran: $<"
	$(FC) $(FFLG) $(PICFLAGS) \
	    $(FDEF) $(FINC) \
	    $(MODOUT) \
	    -c $< -o $@


# ------------------------------------------------------------
# Compile C sources
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.c | $(OBJDIR)
	@echo "Compiling C: $<"
	$(CC) $(CFLG) $(PICFLAGS) \
	    $(CDEF) $(CINC) \
	    -c $< -o $@


# ------------------------------------------------------------
# Optional Fortran dependencies
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
	@rm -f $(MODDIR)/mds_dummy*.mod


# ------------------------------------------------------------
# Generate Fortran dependencies
# ------------------------------------------------------------

depend:
	@if command -v makedepf90 >/dev/null 2>&1; then \
	    makedepf90 -b OBJDIR \
	        $(SRCFOR) $(SRCF) $(SRCF90) > make.depend; \
	    sed -i 's|OBJDIR|$$(OBJDIR)|g' make.depend; \
	else \
	    echo "WARNING: makedepf90 is not available"; \
	fi


# ============================================================
# End of Makefile
# ============================================================
