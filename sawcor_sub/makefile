# ---------------------------------------------------------------------------
# Makefile: sawcor_sub
#
# Builds:
#   libsawcor_sub.a
#   libsawcor_sub.so   when MAKE_SO=1
#
# Shared-library dependencies:
#   libureadsub
#   libtrgraf
#   libr4smlib_userio
#   libr4smlib
# ---------------------------------------------------------------------------

# Include system/compiler dependent flags
include $(COMPILER_FLAGS)

NAME := sawcor_sub

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Sources
# ---------------------------------------------------------------------------

SRCF := $(wildcard *.for)
OBJF := $(SRCF:%.for=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# Fortran flags
# ---------------------------------------------------------------------------

FDEF := $(FDEFS)

FINC := $(FINCL) \
	-I$(MODDIR)

ifeq ($(MAKE_SO),1)
FPIC := -fPIC
else
FPIC :=
endif

FFLG := $(FFLAGS) $(FPIC)


# ---------------------------------------------------------------------------
# Shared-library dependencies
#
# sawcor_sub directly uses routines from:
#
#   libureadsub
#   libtrgraf
#   libr4smlib_userio
#   libr4smlib
#
# Each direct dependency is explicitly linked so that it is recorded
# in the DT_NEEDED entries of libsawcor_sub.so.
# ---------------------------------------------------------------------------

L_UFILES_DEP := \
	-L$(LIBDIR) \
	-lureadsub \
	-ltrgraf \
	-lr4smlib_userio \
	-lr4smlib

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
		$(L_UFILES_DEP) \
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
	$(FC) $(FPP) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


# ---------------------------------------------------------------------------
# Clean
# ---------------------------------------------------------------------------

clean:
	@rm -f $(OBJDIR)/*.o


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
