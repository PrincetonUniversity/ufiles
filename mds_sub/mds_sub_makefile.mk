# Makefile to build libmds_sub.a
#

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := mds_sub
OBJDIR := $(PREFIX)/obj/$(NAME)
LIBAR := $(PREFIX)/lib/lib$(NAME).a
LIBSO := $(PREFIX)/lib/lib$(NAME).so

MNMS := mmds_access.mod mds_readnode.mod
MODS := mds_access.o mds_readnode.o
SRCM := mds_access.for mds_readnode.f90
SF90 := $(SRCM) $(filter-out $(SRCM), $(wildcard *.f90))
SFOR := $(filter-out $(SRCM), $(wildcard *.for))
SRCF := $(SF90) $(SFOR)
OBJF := $(SRCF:%.f90=$(OBJDIR)/%.o)
OBJF := $(OBJF:%.for=$(OBJDIR)/%.o)

FDEF := $(FDEFS)
FINC := $(FINCL)
ifdef MDSPLUS_DIR
    FINC := $(FINC) -I$(MDSPLUS_DIR)/include
endif
FFLG := $(FFLAGS)


.PHONY: clean realclean clobber check

all:	check $(NAME)

check:
	@test -d $(PREFIX)/lib || mkdir -p $(PREFIX)/lib
	@test -d $(PREFIX)/obj/$(NAME) || mkdir -p $(PREFIX)/obj/$(NAME)

$(NAME): $(OBJF)
	@echo "Building $(NAME) static library"
	@$(AR) $(LIBAR) $(OBJF)
ifeq ($(MAKE_SO),1)
	@echo "Building $(NAME) shared library"
	@$(FC) -shared $(LDFLAGS) -o $(LIBSO) $(OBJF)
endif

include $(NAME)_depend.mk
$(OBJDIR)/%.o: %.for
	@$(FC) $(FPP) $(FFLG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod
$(OBJDIR)/%.o: %.f90
	@$(FC) $(FPP) $(FFLG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod

clean:
	@rm -f $(OBJDIR)/*.o
	@cd $(PREFIX)/mod ; rm -f $(MNMS)

realclean: clean
	@rm -f $(LIBAR) $(LIBSO)

clobber: realclean

depend:
	@makedepf90 -b OBJDIR $(SRCF) > $(NAME)_depend.mk
	@sed -i 's/OBJDIR/\$$\(OBJDIR\)/g' $(NAME)_depend.mk
