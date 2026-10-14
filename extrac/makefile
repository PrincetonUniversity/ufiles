
# Makefile to build extrac executable
#
# Supports:
#   Intel oneAPI 2023 (ifort)
#   GNU GCC 13.x (gfortran)
#
# Required:
#   PREFIX
#   COMPILER_FLAGS
#
# Output:
#   $(PREFIX)/bin/extrac
#

# ------------------------------------------------------------
# Include system-dependent compiler and linker flags
# ------------------------------------------------------------

include $(COMPILER_FLAGS)

NAME   := extrac
LIBDIR := $(PREFIX)/lib
OBJDIR := $(PREFIX)/obj/$(NAME)
EXEDIR := $(PREFIX)/bin
MODDIR := $(PREFIX)/mod

EXE := $(EXEDIR)/$(NAME)
OBJ := $(OBJDIR)/$(NAME).o

# ------------------------------------------------------------
# UFILES libraries
# ------------------------------------------------------------

LIBS := \
    extrac_sub \
    interp_sub \
    rngsel_sub \
    r4smlib_userio \
    uflib_userio \
    uflib \
    mds_sub \
    mdstransp \
    ufhdf \
    trgraf \
    ureadsub \
    r4smlib \
    tokid \
    ufencod \
    sg \
    jc \
    ufport

L_UFILES := -L$(LIBDIR) $(addprefix -l,$(LIBS))

# ------------------------------------------------------------
# External libraries
# ------------------------------------------------------------

L_LIBS := \
    $(L_UFILES) \
    $(L_PSPLINE) \
    $(L_NETCDF) \
    $(L_MDSPLUS) \
    $(L_EDIT)

# ------------------------------------------------------------
# Compiler flags
# ------------------------------------------------------------

FDEF := $(FDEFS)
FINC := $(FINCL) -I$(MODDIR)
FFLG := $(FFLAGS)

# ------------------------------------------------------------
# Runtime library search paths
# ------------------------------------------------------------

RPATH_EXTRAC := -Wl,-rpath,'$$ORIGIN/../lib'

ifneq ($(strip $(PSPLINE_HOME)),)
    RPATH_EXTRAC += -Wl,-rpath,$(PSPLINE_HOME)/lib
endif

# ------------------------------------------------------------
# Targets
# ------------------------------------------------------------

.PHONY: all check libs force build rebuild \
        clean realclean clobber

all: $(EXE)

$(NAME): $(EXE)

libs: build $(EXE)

force: rebuild $(EXE)

# ------------------------------------------------------------
# Create directories
# ------------------------------------------------------------

check: | $(EXEDIR) $(OBJDIR)

$(EXEDIR) $(OBJDIR):
	@mkdir -p $@

# ------------------------------------------------------------
# Build dependent libraries
# ------------------------------------------------------------

build:
	@set -e; \
	for l in $(LIBS); do \
	    echo "Building $$l"; \
	    $(MAKE) -C ../$$l; \
	done

rebuild:
	@set -e; \
	for l in $(LIBS); do \
	    echo "Rebuilding $$l"; \
	    $(MAKE) -C ../$$l clobber; \
	    $(MAKE) -C ../$$l; \
	done

# ------------------------------------------------------------
# Compile fixed-form Fortran
#
# IMPORTANT:
#   -c prevents ifort from linking during compilation.
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.for | $(OBJDIR)
	@echo "Compiling $<"
	$(FC) $(FFLG) $(FDEF) $(FINC) \
	    -c $< -o $@

# ------------------------------------------------------------
# Link executable
# ------------------------------------------------------------

$(EXE): $(OBJ) | $(EXEDIR)
	@echo "Building $(EXE)"
	$(FC) $(LDFLAGS) \
	    -o $@ \
	    $(OBJ) \
	    $(L_LIBS) \
	    $(RPATH_EXTRAC)

# ------------------------------------------------------------
# Clean
# ------------------------------------------------------------

clean:
	@echo "Cleaning $(NAME)"
	@rm -f $(OBJDIR)/*.o
	@rm -f $(EXE)

realclean: clean

clobber: realclean
