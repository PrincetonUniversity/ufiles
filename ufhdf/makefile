# ---------------------------------------------------------------------------
# Makefile: ufhdf
#
# Builds:
#   libufhdf.a
#   libufhdf.so   when MAKE_SO=1
#
# Shared-library dependencies:
#   libufencod
#   libufport
#   NetCDF Fortran/C
# ---------------------------------------------------------------------------

# Include system/compiler dependent flags
include $(COMPILER_FLAGS)

NAME := ufhdf

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Fortran modules and sources
# ---------------------------------------------------------------------------

MNMS := ufiles.mod
MODS := ufiles_mod.o

SRCM := $(MODS:%.o=%.f90)

SF90 := $(SRCM) $(filter-out $(SRCM),$(wildcard *.f90))
SFOR := $(wildcard *.for)

SRCF := $(SF90) $(SFOR)

OBJF := $(SF90:%.f90=$(OBJDIR)/%.o)
OBJF += $(SFOR:%.for=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# Fortran flags
# ---------------------------------------------------------------------------

FDEF := $(FDEFS)

FINC := $(FINCL) \
	-I$(MODDIR) \
	-I$(shell nf-config --includedir)

ifeq ($(MAKE_SO),1)
FPIC := -fPIC
else
FPIC :=
endif

FFLG := $(FFLAGS) $(FPIC)


# ---------------------------------------------------------------------------
# Shared-library dependencies
#
# ufhdf uses routines from:
#   libufencod
#   libufport
#   NetCDF Fortran/C
#
# Libraries are listed after the objects that require them.
# ---------------------------------------------------------------------------

L_UFILES_DEP := \
	-L$(LIBDIR) \
	-lufencod \
	-lufport

L_LIBS := \
	$(L_UFILES_DEP) \
	$(L_NETCDF)


# ---------------------------------------------------------------------------
# Runtime search path
#
# UFILES shared libraries are installed together in $(LIBDIR).
# $ORIGIN tells the dynamic loader to search the directory containing
# libufhdf.so for sibling UFILES shared libraries such as:
#
#   libufencod.so
#   libufport.so
# ---------------------------------------------------------------------------

SHARED_RPATH := -Wl,-rpath,'$$ORIGIN'


# ---------------------------------------------------------------------------
# Targets
# ---------------------------------------------------------------------------

.PHONY: all clean realclean clobber check depend

all: check $(NAME)


# ---------------------------------------------------------------------------
# Create output directories
# ---------------------------------------------------------------------------

check:
	@mkdir -p $(LIBDIR)
	@mkdir -p $(OBJDIR)
	@mkdir -p $(MODDIR)


# ---------------------------------------------------------------------------
# Libraries
# ---------------------------------------------------------------------------

$(NAME): $(OBJF)

	@echo "Building $(NAME) static library"
	@rm -f $(LIBAR)
	$(AR) $(LIBAR) $(OBJF)

ifeq ($(MAKE_SO),1)
	@echo "Building $(NAME) shared library"
	@rm -f $(LIBSO)
	$(FC) -shared $(LDFLAGS) \
		-o $(LIBSO) \
		$(OBJF) \
		$(L_LIBS) \
		$(SHARED_RPATH)
endif


# ---------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------

include make.depend


# ---------------------------------------------------------------------------
# Fortran compilation
# ---------------------------------------------------------------------------

$(OBJDIR)/%.o: %.for | check
	$(FC) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


$(OBJDIR)/%.o: %.f90 | check
	$(FC) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


# ---------------------------------------------------------------------------
# Clean
# ---------------------------------------------------------------------------

clean:
	@rm -f $(OBJDIR)/*.o
	@rm -f $(addprefix $(MODDIR)/,$(MNMS))


realclean: clean
	@rm -f $(LIBAR)
	@rm -f $(LIBSO)


clobber: realclean


# ---------------------------------------------------------------------------
# Generate Fortran dependencies
# ---------------------------------------------------------------------------

depend:
	@makedepf90 -b OBJDIR $(SRCF) > make.depend
	@sed -i 's/OBJDIR/\$$\(OBJDIR\)/g' make.depend
