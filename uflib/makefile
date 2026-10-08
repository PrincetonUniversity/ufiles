# ---------------------------------------------------------------------------
# Makefile: uflib
#
# Builds:
#   libuflib.a
#   libuflib.so   when MAKE_SO=1
#
# Shared-library dependencies:
#   libufhdf
#   libmds_sub
#   libmds_dummy
#   libtokid
#   libureadsub
#   libufencod
#   libufport
# ---------------------------------------------------------------------------

# Include system/compiler dependent flags
include $(COMPILER_FLAGS)

NAME := uflib

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

SRCC := $(wildcard *.c)
OBJC := $(SRCC:%.c=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# Fortran flags
# ---------------------------------------------------------------------------

FDEF := $(FDEFS)

FINC := $(FINCL) \
	-I$(MODDIR) \
	-I$(shell nf-config --includedir)

ifdef MDSPLUS_DIR
FINC += -I$(MDSPLUS_DIR)/include
endif


# ---------------------------------------------------------------------------
# C flags
# ---------------------------------------------------------------------------

CDEF := $(CDEFS)

CINC := $(CINCL) \
	-I$(UFILES_HOME)/include


# ---------------------------------------------------------------------------
# Position-independent code
# ---------------------------------------------------------------------------

ifeq ($(MAKE_SO),1)
FPIC := -fPIC
else
FPIC :=
endif

FFLG := $(FFLAGS) $(FPIC)
CFLG := $(CFLAGS) $(FPIC)


# ---------------------------------------------------------------------------
# Shared-library dependencies
#
# uflib directly uses routines from:
#
#   libufhdf
#       UFILES module variables, ucdf*, cdfopen, cdfclos,
#       ufinit, ufsnam, etc.
#
#   libmds_sub
#       add1duf, add2duf, add3duf, addaepuf,
#       add_inf, add_file, mds_whdr, mdsmsg,
#       mds_wr_source_shot, ck_ufpath, uf_fin, etc.
#
#   libmds_dummy
#       idescr_*, mdsput, mdsvalue, mdsfconnect,
#       mds_value, mds_errstr, mds_open, etc.
#
#   libtokid
#       mkbeastid
#
#   libureadsub
#       uinita, uermsg
#
#   libufencod
#       ubi3ncod, packordr, xpand2, ufopen,
#       ubufo, vmsieee, ubufi, ubi3dcod, cmprs2
#
#   libufport
#       uupper, cftclose, ufierfnof, ufbad_exit,
#       sget_env, frename, c9date, cftrewnd,
#       nanscan_r4, ulower, etc.
# ---------------------------------------------------------------------------

L_UFILES_DEP := \
	-L$(LIBDIR) \
	-lufhdf \
	-lmds_sub \
	-lmds_dummy \
	-ltokid \
	-lureadsub \
	-lufencod \
	-lufport

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
# C compilation
# ---------------------------------------------------------------------------

$(OBJDIR)/%.o: %.c | check
	$(CC) $(CFLG) -c $< -o $@ \
		$(CDEF) $(CINC)


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
