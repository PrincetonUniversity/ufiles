# ---------------------------------------------------------------------------
# Makefile: trgraf
#
# Builds:
#   libtrgraf.a
#   libtrgraf.so   when MAKE_SO=1
#
# Shared-library dependencies:
#   libureadsub
#   libufencod
#   libufport
#   libsg
#   libsgdummy
# ---------------------------------------------------------------------------

# Include system/compiler dependent flags
include $(COMPILER_FLAGS)

NAME := trgraf

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Fortran modules and sources
# ---------------------------------------------------------------------------

MNMS := \
	bottoms_up.mod \
	trgraf_wname_save.mod \
	gftic.mod \
	mulfdr_blk.mod \
	gscale_blk.mod \
	intlab.mod \
	gaxes_blk.mod \
	plot1mmx.mod \
	plot1mmxb.mod

MODS := \
	bottoms_up.o \
	trgraf_wname_save.o \
	gftic.o \
	mulfdr_blk.o \
	gscale_blk.o \
	intlab.o \
	gaxes_blk.o \
	plot1mmx.o

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
# trgraf directly uses routines from:
#
#   libureadsub
#       uinita, uchkop, lnout, ugmode, urdcmi,
#       idcod, urdtty, uermsf, uread_freelun,
#       ixuopc, ubinit, fdcod, uermsg, uclear,
#       cdcod, ugline, ureadi, ureadw, unixfile,
#       ureadf, lxout, inord, lyes, neupag,
#       uread, uamode, upaust, urd_setprog,
#       uermsi, udisp1, lchar, upcget
#
#   libufencod
#       ilnurd
#
#   libufport
#       uupper, ufbad_exit, ulower
#
#   libsg
#       graphics routines such as ibasey, label,
#       agiynt, agidlz, linclr, agixty, etc.
#
#   libsgdummy
#       graphics stubs such as dlimy, dsplay,
#       xfrm, line, slimx, etc.
# ---------------------------------------------------------------------------

L_UFILES_DEP := \
	-L$(LIBDIR) \
	-lureadsub \
	-lufencod \
	-lufport \
	-lsg \
	-lsgdummy

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
