# Makefile to build splice2 executable
#

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := splice2
LIBDIR := $(PREFIX)/lib
OBJDIR := $(PREFIX)/obj/$(NAME)
EXEDIR := $(PREFIX)/bin

LIBS := splice_sub interp_sub r4smlib_userio uflib_userio uflib \
	mds_sub mdstransp ufhdf trgraf ureadsub r4smlib tokid ufencod \
	sg jc ufport

L_UFILES := -L$(LIBDIR) $(addprefix -l, $(LIBS))

L_LIBS := $(L_UFILES) $(L_NETCDF) $(L_MDSPLUS) $(L_EDIT)

FDEF := $(FDEFS) -DVERSION=$(VERSNUM) -DGIT_HASH=$(GITHASH) -DBUILD_DATE=$(BUILDDATE)
FINC := $(FINCL) -I$(PREFIX)/mod
FFLG := $(FFLAGS)


.PHONY: clean realclean clobber check libs force build rebuild

all:	check $(NAME)

libs:	check build $(NAME)

force:	check rebuild $(NAME)

build:
	@set -e; for l in $(LIBS); do cd ../$${l}; make -f $${l}_makefile.mk -j; done

rebuild:
	@set -e; for l in $(LIBS); do cd ../$${l}; make -f $${l}_makefile.mk clobber; make -f $${l}_makefile.mk -j; done

check:
	@test -d $(EXEDIR) || mkdir -p $(EXEDIR)
	@test -d $(OBJDIR) || mkdir -p $(OBJDIR)

$(NAME): $(OBJDIR)/splice2.o
	@echo "Building $(NAME) executable"
	@$(FC) $(LDFLAGS) $< -o $(EXEDIR)/$@ $(L_LIBS)

$(OBJDIR)/%.o: %.for
	@$(FC) $(FFLG) $< -o $@ $(FDEF) $(FINC)

clean:
	@rm -f $(OBJDIR)/*.o
	@rm -f $(EXEDIR)/$(NAME)

realclean: clean

clobber: realclean
