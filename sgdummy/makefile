# ======================================================================
# Makefile to build libsgdummy.a and libsgdummy.so
#
# UFILES - Generic GNU/Linux
#
# Compiler configuration:
#     $(COMPILER_FLAGS)
#
# MAKE_SO=0 : Build static library only
# MAKE_SO=1 : Build static and shared libraries
#
# Compatible with:
#     GCC 11   - GitHub Actions
#     GCC 13.2 - PPPL Flux
#     GCC 13.3 - Ranger
#
# The top-level UFILES Makefile invokes:
#
#     make -C sgdummy sgdummy
#
# Therefore, the "sgdummy" target must be preserved.
#
# ======================================================================

# ----------------------------------------------------------------------
# Compiler configuration
# ----------------------------------------------------------------------

ifeq ($(strip $(COMPILER_FLAGS)),)
$(error COMPILER_FLAGS is not defined. Source the UFILES environment first.)
endif

include $(COMPILER_FLAGS)

# ----------------------------------------------------------------------
# Build configuration
# ----------------------------------------------------------------------

NAME := sgdummy

OBJDIR := $(PREFIX)/obj/$(NAME)
LIBDIR := $(PREFIX)/lib
MODDIR := $(PREFIX)/mod

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so

# ----------------------------------------------------------------------
# Sources and objects
# ----------------------------------------------------------------------

SRCF := $(wildcard *.for)
OBJF := $(patsubst %.for,$(OBJDIR)/%.o,$(SRCF))

# ----------------------------------------------------------------------
# Compiler flags
#
# All compiler settings come from COMPILER_FLAGS.
#
# The compiler configuration controls:
#     FC
#     FFLAGS
#     FDEFS
#     FINCL
#     FPP
#     MFLAG
#     AR
#     LDFLAGS
#
# MAKE_SO=1 should enable -fPIC through the compiler configuration.
# ----------------------------------------------------------------------

FDEF := $(FDEFS)
FINC := $(FINCL)
FFLG := $(FFLAGS)

# Ensure position-independent code when building shared libraries.
# This is harmless if FFLAGS already contains -fPIC.

ifeq ($(MAKE_SO),1)
PICFLAG := -fPIC
else
PICFLAG :=
endif

# ----------------------------------------------------------------------
# Targets
# ----------------------------------------------------------------------

.PHONY: all sgdummy static shared check clean realclean clobber depend

.DEFAULT_GOAL := all

all: sgdummy

# Preserve compatibility with the top-level UFILES Makefile.

ifeq ($(MAKE_SO),1)
sgdummy: $(LIBAR) $(LIBSO)
else
sgdummy: $(LIBAR)
endif

static: $(LIBAR)

shared: $(LIBSO)

# ----------------------------------------------------------------------
# Directories
# ----------------------------------------------------------------------

$(LIBDIR) $(OBJDIR) $(MODDIR):
	@mkdir -p $@

check: $(LIBDIR) $(OBJDIR) $(MODDIR)

# ----------------------------------------------------------------------
# Fortran compilation
#
# Explicit -c prevents the compiler from attempting to link an
# executable, which caused the GitHub Actions GCC 11 failure:
#
#     undefined reference to `main'
#
# MFLAG is supplied by the compiler configuration.
# For GNU Fortran:
#
#     MFLAG = -J
#
# ----------------------------------------------------------------------

$(OBJDIR)/%.o: %.for | $(OBJDIR) $(MODDIR)
	@echo "============================================================"
	@echo "Compiling $<"
	@echo "============================================================"
	$(FC) $(FPP) $(FFLG) $(PICFLAG) \
	    $(FDEF) $(FINC) \
	    $(MFLAG)$(MODDIR) \
	    -c $< -o $@

# ----------------------------------------------------------------------
# Static library
#
# The compiler configuration normally defines:
#
#     AR = ar rcs
#
# Do not append another "rcs".
# ----------------------------------------------------------------------

$(LIBAR): $(OBJF) | $(LIBDIR)
	@echo "============================================================"
	@echo "Building $(LIBAR)"
	@echo "============================================================"
	@rm -f $@
	$(AR) $@ $(OBJF)

# ----------------------------------------------------------------------
# Shared library
#
# All objects must be compiled with -fPIC.
# ----------------------------------------------------------------------

$(LIBSO): $(OBJF) | $(LIBDIR)
	@echo "============================================================"
	@echo "Building $(LIBSO)"
	@echo "============================================================"
	$(FC) -shared $(LDFLAGS) \
	    -Wl,-soname,lib$(NAME).so \
	    -o $@ \
	    $(OBJF)

# ----------------------------------------------------------------------
# Dependencies
# ----------------------------------------------------------------------

-include make.depend

depend:
	@makedepf90 -b OBJDIR $(SRCF) > make.depend
	@sed -i 's/OBJDIR/\$$\(OBJDIR\)/g' make.depend

# ----------------------------------------------------------------------
# Cleanup
# ----------------------------------------------------------------------

clean:
	@echo "Cleaning $(NAME) object files"
	@rm -f $(OBJDIR)/*.o

realclean: clean
	@echo "Removing $(NAME) libraries"
	@rm -f $(LIBAR) $(LIBSO)

clobber: realclean

# ======================================================================
# End of Makefile
# ======================================================================
