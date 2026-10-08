# ---------------------------------------------------------------------------
# Makefile: mds_sub
#
# Builds:
#   libmds_sub.a
#   libmds_sub.so   when MAKE_SO=1
#
# Shared-library dependencies:
#   libureadsub
#   libufencod
#   libufport
#   libtokid
#   libmds_dummy
# ---------------------------------------------------------------------------

# Include system/compiler dependent flags
include $(COMPILER_FLAGS)

NAME := mds_sub

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Fortran modules and sources
# ---------------------------------------------------------------------------

MNMS := \
	mmds_access.mod \
	mds_readnode.mod

MODS := \
	mds_access.o \
	mds_readnode.o

SRCM := \
	mds_access.for \
	mds_readnode.f90

SF90 := $(filter %.f90,$(SRCM)) \
	$(filter-out $(filter %.f90,$(SRCM)),$(wildcard *.f90))

SFOR := $(filter %.for,$(SRCM)) \
	$(filter-out $(filter %.for,$(SRCM)),$(wildcard *.for))

SRCF := $(SF90) $(SFOR)

OBJF := $(SF90:%.f90=$(OBJDIR)/%.o)
OBJF += $(SFOR:%.for=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# Compiler flags
# ---------------------------------------------------------------------------

FDEF := $(FDEFS)

FINC := $(FINCL) \
	-I$(MODDIR)

ifdef MDSPLUS_DIR
FINC += -I$(MDSPLUS_DIR)/include
endif

ifeq ($(MAKE_SO),1)
FPIC := -fPIC
else
FPIC :=
endif

FFLG := $(FFLAGS) $(FPIC)


# ---------------------------------------------------------------------------
# Shared-library dependencies
#
# mds_sub directly uses:
#
#   libureadsub
#       uinita, lnout, uermsg, ureadi, ureadw,
#       lxout, uread, lchar
#
#   libufport
#       uupper, get_arg, sget_user, sget_env,
#       ufbad_exit, c9date, ulower, get_arg_count
#
#   libufencod
#       ilnurd
#
#   libtokid
#       checkid, mkbeastid
#
#   libmds_dummy
#       descr, idescr_*, mds_*, mdsvalue,
#       mdsput, mdsfconnect, etc.
# ---------------------------------------------------------------------------

L_UFILES_DEP := \
	-L$(LIBDIR) \
	-lureadsub \
	-lufencod \
	-lufport \
	-ltokid \
	-lmds_dummy

SHARED_RPATH := -Wl,-rpath,'$$ORIGIN'


# ---------------------------------------------------------------------------
# Targets
# ---------------------------------------------------------------------------

.PHONY: all check clean realclean clobber depend

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
# Compilation rules
# ---------------------------------------------------------------------------

$(OBJDIR)/%.o: %.for | check
	$(FC) $(FPP) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


$(OBJDIR)/%.o: %.f90 | check
	$(FC) $(FPP) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


# ---------------------------------------------------------------------------
# Cleanup
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
