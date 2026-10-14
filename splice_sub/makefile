
# Makefile to build:
#   libsplice_sub.a
#   libsplice_sub.so (when MAKE_SO=1)
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
#

# ------------------------------------------------------------
# Include system-dependent compiler and linker flags
# ------------------------------------------------------------

include $(COMPILER_FLAGS)

NAME   := splice_sub
OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR  := $(LIBDIR)/lib$(NAME).a
LIBSO  := $(LIBDIR)/lib$(NAME).so

# ------------------------------------------------------------
# Fortran module sources
# ------------------------------------------------------------

MNMS := splice_blk.mod splice2_blk.mod splice3_blk.mod

MODSRC := splice_blk.f90 splice2_blk.f90 splice3_blk.f90

MODSRC := $(filter $(wildcard *.f90),$(MODSRC))

MODOBJ := $(patsubst %.f90,$(OBJDIR)/%.o,$(MODSRC))

# ------------------------------------------------------------
# Remaining Fortran sources
# ------------------------------------------------------------

SRCF90 := $(filter-out $(MODSRC),$(wildcard *.f90))
SRCFOR := $(wildcard *.for)

OBJF90 := $(patsubst %.f90,$(OBJDIR)/%.o,$(SRCF90))
OBJFOR := $(patsubst %.for,$(OBJDIR)/%.o,$(SRCFOR))

SRCF := $(MODSRC) $(SRCF90) $(SRCFOR)
OBJF := $(MODOBJ) $(OBJF90) $(OBJFOR)

# ------------------------------------------------------------
# Compiler flags
# ------------------------------------------------------------

FDEF := $(FDEFS)
FINC := $(FINCL)

ifeq ($(MAKE_SO),1)
    FPIC := -fPIC
else
    FPIC :=
endif

FFLG := $(FFLAGS) $(FPIC)

# ------------------------------------------------------------
# Shared-library runtime search path
# ------------------------------------------------------------

RPATH_SPLICE := -Wl,-rpath,'$$ORIGIN'

# ------------------------------------------------------------
# Targets
# ------------------------------------------------------------

.PHONY: all check $(NAME) clean realclean clobber depend

all: $(NAME)

$(NAME): $(LIBAR) $(if $(filter 1,$(MAKE_SO)),$(LIBSO))
	@echo "Finished building $(NAME)"

# ------------------------------------------------------------
# Create required directories
# ------------------------------------------------------------

check: | $(LIBDIR) $(OBJDIR) $(MODDIR)

$(LIBDIR) $(OBJDIR) $(MODDIR):
	@mkdir -p $@

# ------------------------------------------------------------
# Static library
#
# AR is already defined as "ar rcs" in COMPILER_FLAGS.
# Do not append another "rcs".
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
	    $(RPATH_SPLICE)

endif

# ------------------------------------------------------------
# Module dependencies
#
# Compile the three module sources before the remaining
# Fortran sources.
#
# Dependencies between the three module sources themselves
# should be specified in make.depend.
# ------------------------------------------------------------

$(OBJF90) $(OBJFOR): $(MODOBJ)

# ------------------------------------------------------------
# Fortran 90 compilation
#
# IMPORTANT: -c prevents ifort from invoking the linker.
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
# Fortran dependency file
# ------------------------------------------------------------

-include make.depend

# ------------------------------------------------------------
# Clean objects and generated module files
# ------------------------------------------------------------

clean:
	@echo "Cleaning $(NAME) objects and modules"
	@rm -f $(OBJDIR)/*.o
	@cd $(MODDIR) 2>/dev/null && rm -f $(MNMS) || true

# ------------------------------------------------------------
# Remove libraries
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
