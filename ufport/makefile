# ---------------------------------------------------------------------------
# Makefile: ufport
#
# Builds:
#   libufport.a
#   libufport.so   when MAKE_SO=1
#
# External shared-library dependency:
#   GNU Readline
# ---------------------------------------------------------------------------

# Include system/compiler dependent flags
include $(COMPILER_FLAGS)

NAME := ufport

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Sources
# ---------------------------------------------------------------------------

SF90 := $(wildcard *.f90)
SFOR := $(wildcard *.for)

SRCF := $(SF90) $(SFOR)

OBJF := $(SF90:%.f90=$(OBJDIR)/%.o)
OBJF += $(SFOR:%.for=$(OBJDIR)/%.o)

SRCC := $(wildcard *.c)
OBJC := $(SRCC:%.c=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# Fortran flags
# ---------------------------------------------------------------------------

FDEF := $(FDEFS)
FINC := $(FINCL)


# ---------------------------------------------------------------------------
# C flags
# ---------------------------------------------------------------------------

CDEF := $(CDEFS) -D__GETLINE_EDITOR
CINC := $(CINCL) -I$(UFILES_HOME)/include


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
# External libraries
#
# ufport C routines use:
#   readline()
#   add_history()
#   history_list()
# ---------------------------------------------------------------------------

L_READLINE := -lreadline


# ---------------------------------------------------------------------------
# Version information
# ---------------------------------------------------------------------------

VERSNUM  := "'"$(shell git describe --tags)"'"
GITHASH  := "'"$(shell git rev-parse --short HEAD)"'"
BUILDDATE := "'"$(shell date +%x)"'"


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
		$(L_READLINE)
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


$(OBJDIR)/ufheader.o: ufheader.f90 | check
	$(FC) $(FPP) $(FFLG) -c $< -o $@ \
		$(FDEF) \
		-DVERSION=$(VERSNUM) \
		-DGIT_HASH=$(GITHASH) \
		-DBUILD_DATE=$(BUILDDATE) \
		$(FINC) \
		$(MFLAG) $(MODDIR)


$(OBJDIR)/%.o: %.f90 | check
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
