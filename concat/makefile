
# Makefile to build concat executable
#
# Supports Intel oneAPI 2023 and GNU GCC 13.x.
#
# Required:
#   PREFIX
#   COMPILER_FLAGS
#
# Output:
#   $(PREFIX)/bin/concat

include $(COMPILER_FLAGS)

NAME   := concat
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
    concat_sub \
    uflib_userio \
    uflib \
    mds_sub \
    mdstransp \
    ufhdf \
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
# Runtime library search path
# ------------------------------------------------------------

RPATH_CONCAT := -Wl,-rpath,'$$ORIGIN/../lib'

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
# Compile Fortran source
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
#
# Object first, followed by libraries.
# ------------------------------------------------------------

$(EXE): $(OBJ) | $(EXEDIR)
	@echo "Building $(EXE)"
	$(FC) $(LDFLAGS) \
	    -o $@ \
	    $(OBJ) \
	    $(L_LIBS) \
	    $(RPATH_CONCAT)

# ------------------------------------------------------------
# Clean
# ------------------------------------------------------------

clean:
	@echo "Cleaning $(NAME)"
	@rm -f $(OBJDIR)/*.o
	@rm -f $(EXE)

realclean: clean

clobber: realclean
