
# Makefile to build cmp_2d executable
#
# Supports:
#   Intel oneAPI 2023 (ifort)
#   GNU GCC 13.x (gfortran)
#
# Required:
#   PREFIX
#   COMPILER_FLAGS
#
# Builds:
#   $(PREFIX)/bin/cmp_2d
#

# ------------------------------------------------------------
# Include system-dependent compiler and linker flags
# ------------------------------------------------------------

include $(COMPILER_FLAGS)

NAME   := cmp_2d

LIBDIR := $(PREFIX)/lib
OBJDIR := $(PREFIX)/obj/$(NAME)
EXEDIR := $(PREFIX)/bin
MODDIR := $(PREFIX)/mod

EXE    := $(EXEDIR)/$(NAME)
OBJF   := $(OBJDIR)/$(NAME).o

# ------------------------------------------------------------
# UFILES libraries
#
# Preserve the original library list and order.
# ------------------------------------------------------------

LIBS := \
    uflib_userio \
    uflib \
    mds_sub \
    ufhdf \
    mdstransp \
    trgraf \
    ureadsub \
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

RPATH_CMP := \
    -Wl,-rpath,'$$ORIGIN/../lib'

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
# Create required directories
# ------------------------------------------------------------

check: | $(EXEDIR) $(OBJDIR)

$(EXEDIR) $(OBJDIR):
	@mkdir -p $@

# ------------------------------------------------------------
# Build dependent libraries
#
# Use the top-level UFILES build to establish library
# dependency order before building this executable.
#
# The standalone build target below retains the original
# per-library invocation convention.
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
# Compile Fortran source
#
# IMPORTANT:
#   -c prevents ifort from invoking the linker.
#
# Without -c, the compiler attempts to resolve
# external UFILES routines during object compilation.
# ------------------------------------------------------------

$(OBJDIR)/%.o: %.for | $(OBJDIR)
	@echo "Compiling $<"
	$(FC) $(FFLG) $(FDEF) $(FINC) \
	    -c $< -o $@

# ------------------------------------------------------------
# Link cmp_2d executable
#
# Object files must appear before the libraries.
# ------------------------------------------------------------

$(EXE): $(OBJF) | $(EXEDIR)
	@echo "Building $(EXE)"
	$(FC) $(LDFLAGS) \
	    -o $@ \
	    $(OBJF) \
	    $(L_LIBS) \
	    $(RPATH_CMP)

# ------------------------------------------------------------
# Clean
# ------------------------------------------------------------

clean:
	@echo "Cleaning $(NAME)"
	@rm -f $(OBJDIR)/*.o
	@rm -f $(EXE)

realclean: clean

clobber: realclean
