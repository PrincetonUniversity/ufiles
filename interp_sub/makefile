
# Makefile to build:
#   libinterp_sub.a
#   libinterp_sub.so (when MAKE_SO=1)
#
# Supports:
#   Intel oneAPI 2023 (ifort)
#   GNU GCC 13.x (gfortran)
#
# Required environment:
#   PREFIX
#   COMPILER_FLAGS
#
# Optional:
#   MAKE_SO=1
#   L_PSPLINE
#   PSPLINE_HOME
#

# ------------------------------------------------------------
# Include system-dependent compiler and linker flags
# ------------------------------------------------------------

include $(COMPILER_FLAGS)

NAME   := interp_sub
OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR  := $(LIBDIR)/lib$(NAME).a
LIBSO  := $(LIBDIR)/lib$(NAME).so

# ------------------------------------------------------------
# Sources and objects
# ------------------------------------------------------------

SRCF90 := $(wildcard *.f90)
SRCFOR := $(wildcard *.for)

SRCF := $(SRCF90) $(SRCFOR)

OBJF90 := $(patsubst %.f90,$(OBJDIR)/%.o,$(SRCF90))
OBJFOR := $(patsubst %.for,$(OBJDIR)/%.o,$(SRCFOR))

OBJF := $(OBJF90) $(OBJFOR)

# ------------------------------------------------------------
# Compiler flags
# ------------------------------------------------------------

FDEF := $(FDEFS)
FINC := $(FINCL)

# Always compile position-independent objects when building
# shared libraries.
#
# MAKE_SO=1:
#   builds static and shared libraries
#
# MAKE_SO=0:
#   builds only the static library
#
ifeq ($(MAKE_SO),1)
    FPIC := -fPIC
else
    FPIC :=
endif

FFLG := $(FFLAGS) $(FPIC)

# ------------------------------------------------------------
# PSPLINE dependency
#
# L_PSPLINE may be supplied by COMPILER_FLAGS.
#
# If not supplied, use PSPLINE_HOME when available.
# ------------------------------------------------------------

ifeq ($(strip $(L_PSPLINE)),)
ifneq ($(strip $(PSPLINE_HOME)),)
    L_PSPLINE := -L$(PSPLINE_HOME)/lib -lpspline
endif
endif

# ------------------------------------------------------------
# Shared-library runtime search paths
#
# $$ORIGIN is expanded by the runtime linker to the
# directory containing libinterp_sub.so.
# ------------------------------------------------------------

RPATH_INTERP := -Wl,-rpath,'$$ORIGIN'

ifneq ($(strip $(PSPLINE_HOME)),)
    RPATH_INTERP += -Wl,-rpath,$(PSPLINE_HOME)/lib
endif

# ------------------------------------------------------------
# Targets
# ------------------------------------------------------------

.PHONY: all check $(NAME) clean realclean clobber depend

all: $(NAME)

$(NAME): $(LIBAR) $(if $(filter 1,$(MAKE_SO)),$(LIBSO))
	@echo "Finished building $(NAME)"

# ------------------------------------------------------------
# Create directories
# ------------------------------------------------------------

check: | $(LIBDIR) $(OBJDIR) $(MODDIR)

$(LIBDIR) $(OBJDIR) $(MODDIR):
	@mkdir -p $@

# ------------------------------------------------------------
# Static library
# ------------------------------------------------------------

$(LIBAR): $(OBJF) | $(LIBDIR)
	@echo "Building $(LIBAR)"
	$(AR) $@ $(OBJF)

# ------------------------------------------------------------
# Shared library
# ------------------------------------------------------------

ifeq ($(MAKE_SO),1)

$(LIBSO): $(OBJF) | $(LIBDIR)
	@echo "Building $(LIBSO)"
	$(FC) -shared $(LDFLAGS) \
	    -o $@ \
	    $(OBJF) \
	    $(L_PSPLINE) \
	    $(RPATH_INTERP)

endif

# ------------------------------------------------------------
# Fortran 90 compilation
#
# IMPORTANT:
#   -c is mandatory.
#
# Without -c, ifort attempts to link an executable and
# produces errors such as:
#
#   undefined reference to MAIN__
#   undefined reference to spline_
#   undefined reference to speval_
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.f90 | $(OBJDIR) $(MODDIR)
	@echo "Compiling $<"
	$(FC) $(FFLG) $(FDEF) $(FINC) \
	    $(MFLAG) $(MODDIR) \
	    -c $< -o $@

# ------------------------------------------------------------
# Fixed-form Fortran compilation
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.for | $(OBJDIR) $(MODDIR)
	@echo "Compiling $<"
	$(FC) $(FFLG) $(FDEF) $(FINC) \
	    $(MFLAG) $(MODDIR) \
	    -c $< -o $@

# ------------------------------------------------------------
# Fortran dependencies
# ------------------------------------------------------------

-include make.depend

# ------------------------------------------------------------
# Clean object files
# ------------------------------------------------------------

clean:
	@echo "Cleaning $(NAME) objects"
	@rm -f $(OBJDIR)/*.o

# ------------------------------------------------------------
# Remove objects and libraries
# ------------------------------------------------------------

realclean: clean
	@echo "Removing $(NAME) libraries"
	@rm -f $(LIBAR) $(LIBSO)

clobber: realclean

# ------------------------------------------------------------
# Generate Fortran dependencies
# ------------------------------------------------------------

depend:
	@makedepf90 -b OBJDIR $(SRCF) > make.depend
	@sed -i 's/OBJDIR/\$$\(OBJDIR\)/g' make.depend
