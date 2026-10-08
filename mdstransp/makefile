# ---------------------------------------------------------------------------
# Makefile: mdstransp
#
# Builds:
#   libmdstransp.a
#   libmdstransp.so   when MAKE_SO=1
#
# This library is intentionally compiled without optimization.
#
# Direct UFILES dependency:
#   libufport
#
# External dependency:
#   MDSplus
# ---------------------------------------------------------------------------

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := mdstransp

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Sources
# ---------------------------------------------------------------------------

SRCF := $(wildcard *.f90) $(wildcard *.for)

OBJF := $(SRCF:%.f90=$(OBJDIR)/%.o)
OBJF := $(OBJF:%.for=$(OBJDIR)/%.o)

SRCC := $(wildcard *.c)
OBJC := $(SRCC:%.c=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# Position-independent code
#
# Both Fortran and C objects must be compiled with -fPIC when building
# the shared library.
# ---------------------------------------------------------------------------

ifeq ($(MAKE_SO),1)
FPIC := -fPIC
else
FPIC :=
endif


# ---------------------------------------------------------------------------
# Fortran flags
#
# mdstransp is intentionally compiled using the debug/non-optimized flags.
# ---------------------------------------------------------------------------

FDEF := $(FDEFS)

FINC := $(FINCL) \
	-I$(MODDIR)

FFLG := $(FFLAGS_DBG) $(FPIC)


# ---------------------------------------------------------------------------
# C flags
#
# mdstransp is intentionally compiled using the debug/non-optimized flags.
# ---------------------------------------------------------------------------

CDEF := $(CDEFS)

CINC := $(CINCL) \
	-I$(UFILES_HOME)/include

ifdef MDSPLUS_DIR
CINC := $(CINC) \
	-I$(MDSPLUS_DIR)/include
endif

CFLG := $(CFLAGS_DBG) $(FPIC)


# ---------------------------------------------------------------------------
# Direct UFILES dependencies
#
# Dependency analysis shows:
#
#   uupper_   -> libufport.so
#   cstring_  -> libufport.so
#
# Therefore libufport must be a direct DT_NEEDED dependency of
# libmdstransp.so.
# ---------------------------------------------------------------------------

L_UFILES_DEP := \
	-L$(LIBDIR) \
	-lufport


# ---------------------------------------------------------------------------
# Runtime path
#
# UFILES shared libraries are installed together in $(PREFIX)/lib.
# $ORIGIN allows libmdstransp.so to locate sibling UFILES libraries.
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

$(NAME): $(OBJF) $(OBJC)
	@echo "$(NAME) always compiled without optimization"

	@echo "Building $(NAME) static library"
	@rm -f $(LIBAR)
	$(AR) $(LIBAR) $(OBJF) $(OBJC)

ifeq ($(MAKE_SO),1)
	@echo "Building $(NAME) shared library"
	@rm -f $(LIBSO)
	$(FC) -shared $(LDFLAGS) \
		-o $(LIBSO) \
		$(OBJF) $(OBJC) \
		$(L_UFILES_DEP) \
		$(L_MDSPLUS) \
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
	$(FC) $(FPP) $(FFLG) \
		-c $< \
		-o $@ \
		$(FDEF) \
		$(FINC) \
		$(MFLAG) $(MODDIR)


$(OBJDIR)/%.o: %.f90 | check
	$(FC) $(FPP) $(FFLG) \
		-c $< \
		-o $@ \
		$(FDEF) \
		$(FINC) \
		$(MFLAG) $(MODDIR)


# ---------------------------------------------------------------------------
# C compilation
# ---------------------------------------------------------------------------

$(OBJDIR)/%.o: %.c | check
	$(CC) $(CPP) $(CFLG) \
		-c $< \
		-o $@ \
		$(CDEF) \
		$(CINC)


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
