# GNU Make to build SGLIB and XPLOT
# 08/11/98 C. Ludescher
# 08/18/98 D. Ernst - modified for Portland Group pgf77 v1.7-5
# 09/01/00 CAL : separate f77 / f90
#
# to use debug: make libs DEBUG=y
#
-include ../share/Make.local
include Makefile.def

ifdef SHARED
   MAKE_SO=1
endif
# sglib special flags not covered by Make.flags:

FPP = gcc
CPPFLAGS := $(CPPFLAGS) -traditional

ifdef MKGCC
      FFLAGS :=  $(FFLAGS) -fno-range-check $(STATIC) $(FIXEDE) $(OUT) 
      DFFLAGS := $(DFFLAGS) -fno-range-check $(STATIC) $(FIXEDE) $(OUT)
else
      FFLAGS :=  $(FFLAGS) $(STATIC) $(FIXEDE) $(OUT) 
      DFFLAGS := $(DFFLAGS) $(STATIC) $(FIXEDE) $(OUT)
endif

ifdef DEBUG
	FFLAGS = $(DFFLAGS)
	CFLAGS = $(DCFLAGS)
endif

A=$(PREFIX)/lib
O=$(PREFIX)/obj/sglib
E=$(PREFIX)/bin

# sglib modules
SGOBJ=$(A)/libsg.a(sgiso.o) $(A)/libsg.a(sgnco.o)  $(A)/libsg.a(sgarea.o) \
      $(A)/libsg.a(sgext.o) $(A)/libsg.a(sggrf.o)  $(A)/libsg.a(sgtcs.o)\
      $(A)/libsg.a(sgiof.o) $(A)/libsg.a(sgvarg.o) $(A)/libsg.a(sgsys.o) \
      $(A)/libsg.a(r8sgext.o) $(A)/libsg.a(r8sggrf.o) $(A)/libsg.a(r8sgiso.o) \
      $(A)/libsg.a(r8sgnco.o) $(A)/libsg.a(r8sgtcs.o) $(A)/libsg.a(r4sgext.o) \
      $(A)/libsg.a(r4sggrf.o) $(A)/libsg.a(r4sgnco.o)
# jclib modules
JCOBJ=$(A)/libjc.a(jcmenu.o)   $(A)/libjc.a(jcprompt.o) $(A)/libjc.a(jctic.o) \
      $(A)/libjc.a(jcstring.o) $(A)/libjc.a(jcsys.o)

# Fortran sources
F = $(subst .F,.f, $(wildcard *.F))

# f90 modules
#ifeq "$(findstring f9,$(FC))" "f9"
   Mods = $(subst .f,.o, $(wildcard *_mod.f))
   M := $(foreach mod,$(Mods),$(O)/$(mod))
   M9INC=$(MFLAGS)$(PREFIX)/mod
#else
#   M = 
#   M9INC=
#endif



# always use f90
	FCM := $(FC90) $(M9INC)
	SGDEFS := $(SGDEFS) -D__f90

RLIBS=$(A)/libsg.a $(A)/libjc.a
XLIBS:=$(XLIBS) $(XFLIB)
LIBS=-L$(PREFIX)/lib -ljc -lsg -lufport -L/usr/lib64 -lreadline -lhistory -ltermcap

define cmp_varg
  $(CC) $(CFLAGS) $(SGDEFS) -o $(O)/$% $<
endef

define cmp_sys
  $(CC) $(CFLAGS) $(SGDEFS) -o $(O)/$% $<
endef

# Compile .f
define f_comp
$(FC) $(M9INC) $(FFLAGS) $(O)/$%  $<
@$(AR) $@ $(O)/$%
@if test -z "$(DEBUG)"; then \
	rm -f $(O)/$%; fi
endef

# Function to compile pre-processed .F
define f_proc
$(FC) $(M9INC) -I./ -I$(O) $(FFLAGS) $(O)/$%  $<
@$(AR) $@ $(O)/$%
@if test -z "$(DEBUG)"; then \
	rm -f $(O)/$%; fi
endef

define f90_comp
$(FC) $(M9INC) $(FFLAGS) $(O)/$%  $<
@$(AR) $@ $(O)/$%
@if test -z "$(DEBUG)"; then \
	rm -f $(O)/$%; fi
endef

# Function to compile pre-processed .F
define f90_proc
$(FC) $(M9INC) -I./ -I$(O)/ $(FFLAGS) $(O)/$%  $<
@$(AR) $@ $(O)/$%
@if test -z "$(DEBUG)"; then \
	rm -f $(O)/$%; fi
endef

# Rules
#==================================================================

# libjc.a / libsg.a

libs: FORCE $(M) $(A)/libjc.a $(A)/libsg.a
	@echo made sglib/jclib

all:  FORCE libs exec
	@echo  

$(OLDLIB): timestamp.inf
	@echo "--- DETECTED libsg.a source update"
	@echo "--- Re-Making $(A)/libsg.a"
	@$(MAKE) libs

.PHONY: cleanall clean cleanexe clobber chkdirs exec

# make sure Object directory exists
# if standalone distribution: make ufport
FORCE:  chkdirs
	@if test ! -d $(PREFIX)/mod; then \
	mkdir -p $(PREFIX)/mod; fi
	@if test ! -d $(A); then \
	mkdir $(A); fi
	@if test ! -d $(O); then \
	mkdir $(O); fi


# Pre-process .F
$(O)/%.f: %.F
	@$(FPP) $(CPPFLAGS) $(SGDEFS) - < $< |grep -v '^$$' > $@

# Compile f90 modules
# MFFLAGS should really be same as F90FLAGS ?
$(O)/%_mod.o: %_mod.f
	@$(FC) $(MFFLAGS) $@ $<
	@$(AR) $(A)/libsg.a $@
	@if test $< = sglib_mod.f; then \
	mv *.$(MODEXT) $(PREFIX)/mod/; \
	else mv *.$(MODEXT) $(PREFIX)/mod/; fi


chkdirs:
	@if test ! -d $(O); then \
	mkdir -p $(O); fi

# make sure link exists to sglib.a / jclib.a for TRANSP 
$(A)/libsg.a: $(SGOBJ)
	@$(RANLIB) $@ > /dev/null

$(A)/libjc.a:  $(JCOBJ)
	@$(RANLIB) $@ > /dev/null

# sglib
$(A)/libsg.a(sgiso.o): sgiso.f sgiso_inc
	$(f_comp)

$(A)/libsg.a(sgnco.o): $(O)/sgnco.f sgnco_inc sgtcs_inc
	$(f_proc)

$(A)/libsg.a(sgarea.o): sgarea.f sgarea_inc
	@$(f_comp)

$(A)/libsg.a(sgext.o): $(O)/sgext.f
	@$(f_proc)

$(A)/libsg.a(sggrf.o): $(O)/sggrf.f
	@$(f_proc)

$(A)/libsg.a(sgtcs.o): $(O)/sgtcs.f sgtcs_inc
	@$(f_proc)

$(A)/libsg.a(sgiof.o): $(O)/sgiof.f sgiof_inc sgtcs_inc sgvgds_inc
	@$(f_proc)

$(A)/libsg.a(sgsys.o): sgsys.c sgbuff.h
	@$(cmp_sys)
	@$(AR) $@ $(O)/$%
	@if test -z "$(DEBUG)"; then \
		rm -f $(O)/$%; fi

$(A)/libsg.a(sgvarg.o): sgvarg.c sgvarg_code.h 
	@$(cmp_varg)
	@$(AR) $@ $(O)/$%
	@if test -z "$(DEBUG)"; then \
		rm -f $(O)/$%; fi

$(A)/libsg.a(r8sgiso.o): r8sgiso.f 
	@$(f90_comp)

$(A)/libsg.a(r8sgext.o): r8sgext.f 
	@$(f90_comp)

$(A)/libsg.a(r8sggrf.o): r8sggrf.f 
	@$(f90_comp)

$(A)/libsg.a(r8sgnco.o): r8sgnco.f 
	@$(f90_comp)

$(A)/libsg.a(r8sgtcs.o): r8sgtcs.f 
	@$(f90_comp)

$(A)/libsg.a(r4sgext.o): r4sgext.f 
	@$(f90_comp)

$(A)/libsg.a(r4sggrf.o): r4sggrf.f 
	@$(f90_comp)

$(A)/libsg.a(r4sgnco.o): r4sgnco.f 
	@$(f90_comp)


# jclib
$(A)/libjc.a(jcmenu.o): jcmenu.f jcticcom
	@$(f_comp)

$(A)/libjc.a(jcprompt.o): jcprompt.F jcticcom
	@$(FPP) $(CPPFLAGS) $(SGDEFS) - < $< |grep -v '^$$' > $(O)/$*.f
	@$(FC) -I./ $(FFLAGS) $(O)/$%  $(O)/$*.f
	@$(AR) $@ $(O)/$%
	@if test -z "$(DEBUG)"; then \
		rm -f $(O)/$%; fi

$(A)/libjc.a(jcstring.o): jcstring.f
	@$(f_comp)

$(A)/libjc.a(jctic.o): jctic.f jcticcom
	@$(f_comp)

$(A)/libjc.a(jcsys.o): $(O)/jcsys.f
	@$(f_proc)


# Makefile is divided, to avoid installing test programs, 
# if sglib was included in NTCC module by default
#
-include $(TRINS)Make_install

cleanlib:
	@rm -f $(PREFIX)/lib/libsg.a
	@rm -f $(PREFIX)/lib/libjc.a

cleanexe:
	@rm -f $(PREFIX)/bin/tstp10 $(PREFIX)/bin/tstcon $(PREFIX)/bin/xplot
	@rm -f $(PREFIX)/bin/r4tstcon $(PREFIX)/bin/r4tstp10
	@rm -f $(PREFIX)/bin/r8tstcon $(PREFIX)/bin/r8tstp10

clean:
	@rm -f $(O)/*.o $(O)/*.f
	@rm -f $(PREFIX)/include/*_inc
	@rm -f $(PREFIX)/mod/agcget_mod.mod $(PREFIX)/mod/agcu_mod.mod
	@rm -f $(PREFIX)/mod/sgint_mod.mod $(PREFIX)/mod/sglib_mod.mod
	@rm -f $(PREFIX)/mod/sgnco_mod.mod

clobber: clean cleanlib cleanexe
