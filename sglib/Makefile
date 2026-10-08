# GNU Makefile to build SGLIB and XPLOT
#
# 08/11/98 C. Ludescher
# 08/18/98 D. Ernst - modified for Portland Group pgf77 v1.7-5
# 09/01/00 CAL : separate f77 / f90
#
# Updated to support shared libraries:
#
#       libsg.so
#       libjc.so
#
# Static build:
#
#       gmake libs
#
# Static + shared build:
#
#       gmake MAKE_SO=1 libs
#
# or:
#
#       gmake SHARED=1 libs
#
# Debug:
#
#       gmake DEBUG=y libs
#

-include ../share/Make.local
include Makefile.def


# ======================================================================
# Parallel-build protection
#
# The original SGLIB build uses GNU make archive-member targets such as:
#
#       libsg.a(sgiso.o)
#       libsg.a(sgnco.o)
#
# Multiple parallel "ar" operations on the same archive can corrupt
# libsg.a/libjc.a. Module compilation also produces .mod files in a
# common location.
#
# Keep this Makefile serial. The top-level UFILES build can still use -j.
# ======================================================================

.NOTPARALLEL:


# ======================================================================
# Shared-library selection
# ======================================================================

ifdef SHARED
MAKE_SO = 1
endif

MAKE_SO ?= 0


# ======================================================================
# SGLIB special flags
# ======================================================================

FPP = gcc

CPPFLAGS := $(CPPFLAGS) -traditional


# ----------------------------------------------------------------------
# Save original compiler flags BEFORE adding $(STATIC).
#
# Shared-library objects must be compiled with -fPIC and must NOT inherit
# static-code-generation flags.
# ----------------------------------------------------------------------

BASE_FFLAGS  := $(FFLAGS)
BASE_DFFLAGS := $(DFFLAGS)
BASE_CFLAGS  := $(CFLAGS)
BASE_DCFLAGS := $(DCFLAGS)


ifdef MKGCC

FFLAGS := \
	$(FFLAGS) \
	-fno-range-check \
	$(STATIC) \
	$(FIXEDE) \
	$(OUT)

DFFLAGS := \
	$(DFFLAGS) \
	-fno-range-check \
	$(STATIC) \
	$(FIXEDE) \
	$(OUT)

PIC_FFLAGS := \
	$(BASE_FFLAGS) \
	-fno-range-check \
	$(FIXEDE) \
	-fPIC

PIC_DFFLAGS := \
	$(BASE_DFFLAGS) \
	-fno-range-check \
	$(FIXEDE) \
	-fPIC

else

FFLAGS := \
	$(FFLAGS) \
	$(STATIC) \
	$(FIXEDE) \
	$(OUT)

DFFLAGS := \
	$(DFFLAGS) \
	$(STATIC) \
	$(FIXEDE) \
	$(OUT)

PIC_FFLAGS := \
	$(BASE_FFLAGS) \
	$(FIXEDE) \
	-fPIC

PIC_DFFLAGS := \
	$(BASE_DFFLAGS) \
	$(FIXEDE) \
	-fPIC

endif


PIC_CFLAGS := \
	$(BASE_CFLAGS) \
	-fPIC

PIC_DCFLAGS := \
	$(BASE_DCFLAGS) \
	-fPIC


ifdef DEBUG

FFLAGS     = $(DFFLAGS)
CFLAGS     = $(DCFLAGS)

PIC_FFLAGS = $(PIC_DFFLAGS)
PIC_CFLAGS = $(PIC_DCFLAGS)

endif


# ======================================================================
# Directories
# ======================================================================

A = $(PREFIX)/lib
O = $(PREFIX)/obj/sglib
P = $(PREFIX)/obj/sglib_pic
E = $(PREFIX)/bin


# ======================================================================
# Library names
# ======================================================================

SGLIB_A  = $(A)/libsg.a
JCLIB_A  = $(A)/libjc.a

SGLIB_SO = $(A)/libsg.so
JCLIB_SO = $(A)/libjc.so


# ======================================================================
# Static-library objects
# ======================================================================

SGOBJ = \
	$(A)/libsg.a(sgiso.o) \
	$(A)/libsg.a(sgnco.o) \
	$(A)/libsg.a(sgarea.o) \
	$(A)/libsg.a(sgext.o) \
	$(A)/libsg.a(sggrf.o) \
	$(A)/libsg.a(sgtcs.o) \
	$(A)/libsg.a(sgiof.o) \
	$(A)/libsg.a(sgvarg.o) \
	$(A)/libsg.a(sgsys.o) \
	$(A)/libsg.a(r8sgext.o) \
	$(A)/libsg.a(r8sggrf.o) \
	$(A)/libsg.a(r8sgiso.o) \
	$(A)/libsg.a(r8sgnco.o) \
	$(A)/libsg.a(r8sgtcs.o) \
	$(A)/libsg.a(r4sgext.o) \
	$(A)/libsg.a(r4sggrf.o) \
	$(A)/libsg.a(r4sgnco.o)


JCOBJ = \
	$(A)/libjc.a(jcmenu.o) \
	$(A)/libjc.a(jcprompt.o) \
	$(A)/libjc.a(jctic.o) \
	$(A)/libjc.a(jcstring.o) \
	$(A)/libjc.a(jcsys.o)


# ======================================================================
# Fortran module sources
#
# The static build puts all *_mod.f objects into libsg.a.
# The shared library must contain the same module objects.
# ======================================================================

MODSRC := $(wildcard *_mod.f)

Mods := $(MODSRC:.f=.o)

M := $(foreach mod,$(Mods),$(O)/$(mod))

# PIC copies of all module objects.
MODPICOBJ := $(patsubst %.f,$(P)/%.o,$(MODSRC))


# ======================================================================
# Shared-library PIC objects
# ======================================================================

SGPIC_COREOBJ = \
	$(P)/sgiso.o \
	$(P)/sgnco.o \
	$(P)/sgarea.o \
	$(P)/sgext.o \
	$(P)/sggrf.o \
	$(P)/sgtcs.o \
	$(P)/sgiof.o \
	$(P)/sgvarg.o \
	$(P)/sgsys.o \
	$(P)/r8sgext.o \
	$(P)/r8sggrf.o \
	$(P)/r8sgiso.o \
	$(P)/r8sgnco.o \
	$(P)/r8sgtcs.o \
	$(P)/r4sgext.o \
	$(P)/r4sggrf.o \
	$(P)/r4sgnco.o


SGPICOBJ = \
	$(MODPICOBJ) \
	$(SGPIC_COREOBJ)


JCPICOBJ = \
	$(P)/jcmenu.o \
	$(P)/jcprompt.o \
	$(P)/jctic.o \
	$(P)/jcstring.o \
	$(P)/jcsys.o


# ======================================================================
# Fortran sources/modules
# ======================================================================

F = $(subst .F,.f,$(wildcard *.F))

M9INC = $(MFLAGS)$(PREFIX)/mod

# Always use f90.
FCM := $(FC90) $(M9INC)

SGDEFS := $(SGDEFS) -D__f90


# ======================================================================
# Existing libraries
# ======================================================================

RLIBS = $(A)/libsg.a $(A)/libjc.a

XLIBS := $(XLIBS) $(XFLIB)

LIBS = \
	-L$(PREFIX)/lib \
	-ljc \
	-lsg \
	-lufport \
	-L/usr/lib64 \
	-lreadline \
	-lhistory \
	-ltermcap


# ======================================================================
# Shared-library flags and dependencies
#
# There is a circular dependency:
#
#       libsg.so -> libjc.so
#       libjc.so -> libsg.so
#
# The shared libraries are therefore constructed in three steps:
#
#   1. Build an initial libsg.so against libufport.
#   2. Build libjc.so against the initial libsg.so and libufport.
#   3. Relink libsg.so against libjc.so and libufport.
#
# Final DT_NEEDED relationship:
#
#       libsg.so : libjc.so, libufport.so
#       libjc.so : libsg.so, libufport.so
#
# Readline is a direct dependency of libufport and therefore does not
# need to be duplicated on the sg/jc shared-library link lines.
# ======================================================================

SOFLAGS = -shared

SHARED_RPATH := -Wl,-rpath,'$$ORIGIN'

SG_SHARED_DEP := \
	-L$(PREFIX)/lib \
	-lufport

JC_SHARED_DEP := \
	-L$(PREFIX)/lib \
	-lsg \
	-lufport

SG_FINAL_DEP := \
	-L$(PREFIX)/lib \
	-ljc \
	-lufport


# ======================================================================
# Static-build helper functions
# ======================================================================

define cmp_varg
	$(CC) $(CFLAGS) $(SGDEFS) -o $(O)/$% $<
endef


define cmp_sys
	$(CC) $(CFLAGS) $(SGDEFS) -o $(O)/$% $<
endef


# Compile .f

define f_comp
	$(FC) $(M9INC) $(FFLAGS) $(O)/$% $<
	@$(AR) $@ $(O)/$%
	@if test -z "$(DEBUG)"; then \
		rm -f $(O)/$%; \
	fi
endef


# Compile preprocessed .F

define f_proc
	$(FC) $(M9INC) -I./ -I$(O) $(FFLAGS) $(O)/$% $<
	@$(AR) $@ $(O)/$%
	@if test -z "$(DEBUG)"; then \
		rm -f $(O)/$%; \
	fi
endef


define f90_comp
	$(FC) $(M9INC) $(FFLAGS) $(O)/$% $<
	@$(AR) $@ $(O)/$%
	@if test -z "$(DEBUG)"; then \
		rm -f $(O)/$%; \
	fi
endef


define f90_proc
	$(FC) $(M9INC) -I./ -I$(O) $(FFLAGS) $(O)/$% $<
	@$(AR) $@ $(O)/$%
	@if test -z "$(DEBUG)"; then \
		rm -f $(O)/$%; \
	fi
endef


# ======================================================================
# Main targets
# ======================================================================

ifeq ($(MAKE_SO),1)

libs: FORCE $(M) $(JCLIB_A) $(SGLIB_A) relink_sglib
	@echo "made static and shared sglib/jclib"

else

libs: FORCE $(M) $(JCLIB_A) $(SGLIB_A)
	@echo "made static sglib/jclib"

endif


all: FORCE libs exec
	@echo


$(OLDLIB): timestamp.inf
	@echo "--- DETECTED libsg.a source update"
	@echo "--- Re-Making $(A)/libsg.a"
	@$(MAKE) libs


.PHONY: \
	all \
	libs \
	FORCE \
	relink_sglib \
	chkdirs \
	clean \
	cleanlib \
	cleanshared \
	cleanexe \
	cleanall \
	clobber \
	exec


# ======================================================================
# Directory creation
# ======================================================================

FORCE: chkdirs
	@mkdir -p $(PREFIX)/mod
	@mkdir -p $(A)
	@mkdir -p $(O)
	@mkdir -p $(P)


chkdirs:
	@mkdir -p $(O)
	@mkdir -p $(P)
	@mkdir -p $(A)
	@mkdir -p $(PREFIX)/mod


# ======================================================================
# Preprocessing
# ======================================================================

$(O)/%.f: %.F
	@$(FPP) $(CPPFLAGS) $(SGDEFS) - < $< | \
		grep -v '^$$' > $@


$(P)/%.f: %.F
	@$(FPP) $(CPPFLAGS) $(SGDEFS) - < $< | \
		grep -v '^$$' > $@


# ======================================================================
# Static F90 module compilation
#
# Compile each module and put the generated .mod file directly into
# $(PREFIX)/mod.
# ======================================================================

$(O)/%_mod.o: %_mod.f
	@echo "Compiling static module $<"
	$(FC) \
		$(M9INC) \
		$(FFLAGS) \
		-I$(PREFIX)/mod \
		-J$(PREFIX)/mod \
		-c $< \
		-o $@
	@$(AR) $(A)/libsg.a $@


# ======================================================================
# PIC F90 module compilation
# ======================================================================

$(P)/%_mod.o: %_mod.f
	@echo "Compiling PIC module $<"
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-J$(PREFIX)/mod \
		-c $< \
		-o $@


# Ordinary SGLIB PIC objects may use modules generated above.
$(SGPIC_COREOBJ): $(MODPICOBJ)


# ======================================================================
# Static libraries
# ======================================================================

$(SGLIB_A): $(SGOBJ)
	@$(RANLIB) $@ > /dev/null


$(JCLIB_A): $(JCOBJ)
	@$(RANLIB) $@ > /dev/null


# ======================================================================
# Shared libraries
#
# Because libsg and libjc depend on each other, the final build is:
#
#       initial libsg.so
#              |
#              v
#          libjc.so
#              |
#              v
#       final libsg.so
# ======================================================================


# ----------------------------------------------------------------------
# Step 1: initial libsg.so
#
# This initial version provides the sg symbols needed while linking
# libjc.so. It is replaced by the final version in relink_sglib.
# ----------------------------------------------------------------------

$(SGLIB_SO): $(SGPICOBJ)
	@echo "Building initial shared library $(SGLIB_SO)"
	$(FC) \
		$(SOFLAGS) \
		$(LDFLAGS) \
		-o $@ \
		$(SGPICOBJ) \
		$(SG_SHARED_DEP) \
		$(SHARED_RPATH)


# ----------------------------------------------------------------------
# Step 2: libjc.so
#
# The initial libsg.so must already exist.
# ----------------------------------------------------------------------

$(JCLIB_SO): $(JCPICOBJ) $(SGLIB_SO)
	@echo "Building shared library $(JCLIB_SO)"
	$(FC) \
		$(SOFLAGS) \
		$(LDFLAGS) \
		-o $@ \
		$(JCPICOBJ) \
		$(JC_SHARED_DEP) \
		$(SHARED_RPATH)


# ----------------------------------------------------------------------
# Step 3: final libsg.so
#
# Relink libsg.so after libjc.so exists. This records libjc.so as a
# direct DT_NEEDED dependency of the final libsg.so.
# ----------------------------------------------------------------------

relink_sglib: $(JCLIB_SO)
	@echo "Relinking shared library $(SGLIB_SO) with libjc dependency"
	$(FC) \
		$(SOFLAGS) \
		$(LDFLAGS) \
		-o $(SGLIB_SO) \
		$(SGPICOBJ) \
		$(SG_FINAL_DEP) \
		$(SHARED_RPATH)


# ======================================================================
# PIC objects for libsg.so
# ======================================================================

$(P)/sgiso.o: sgiso.f sgiso_inc
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/sgnco.o: $(P)/sgnco.f sgnco_inc sgtcs_inc $(P)/sgnco_mod.o
	$(FC) \
		$(M9INC) \
		-I./ \
		-I$(P) \
		-I$(PREFIX)/mod \
		$(PIC_FFLAGS) \
		-c $< \
		-o $@


$(P)/sgarea.o: sgarea.f sgarea_inc
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/sgext.o: $(P)/sgext.f
	$(FC) \
		$(M9INC) \
		-I./ \
		-I$(P) \
		-I$(PREFIX)/mod \
		$(PIC_FFLAGS) \
		-c $< \
		-o $@


$(P)/sggrf.o: $(P)/sggrf.f
	$(FC) \
		$(M9INC) \
		-I./ \
		-I$(P) \
		-I$(PREFIX)/mod \
		$(PIC_FFLAGS) \
		-c $< \
		-o $@


$(P)/sgtcs.o: $(P)/sgtcs.f sgtcs_inc
	$(FC) \
		$(M9INC) \
		-I./ \
		-I$(P) \
		-I$(PREFIX)/mod \
		$(PIC_FFLAGS) \
		-c $< \
		-o $@


$(P)/sgiof.o: $(P)/sgiof.f sgiof_inc sgtcs_inc sgvgds_inc
	$(FC) \
		$(M9INC) \
		-I./ \
		-I$(P) \
		-I$(PREFIX)/mod \
		$(PIC_FFLAGS) \
		-c $< \
		-o $@


$(P)/sgvarg.o: sgvarg.c sgvarg_code.h
	$(CC) \
		$(PIC_CFLAGS) \
		$(SGDEFS) \
		-c $< \
		-o $@


$(P)/sgsys.o: sgsys.c sgbuff.h
	$(CC) \
		$(PIC_CFLAGS) \
		$(SGDEFS) \
		-c $< \
		-o $@


$(P)/r8sgiso.o: r8sgiso.f
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/r8sgext.o: r8sgext.f
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/r8sggrf.o: r8sggrf.f
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/r8sgnco.o: r8sgnco.f
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/r8sgtcs.o: r8sgtcs.f
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/r4sgext.o: r4sgext.f
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/r4sggrf.o: r4sggrf.f
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/r4sgnco.o: r4sgnco.f
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


# ======================================================================
# PIC objects for libjc.so
# ======================================================================

$(P)/jcmenu.o: jcmenu.f jcticcom
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/jcprompt.o: jcprompt.F jcticcom
	@$(FPP) $(CPPFLAGS) $(SGDEFS) - < $< | \
		grep -v '^$$' > $(P)/jcprompt.f
	$(FC) \
		$(M9INC) \
		-I./ \
		-I$(PREFIX)/mod \
		$(PIC_FFLAGS) \
		-c $(P)/jcprompt.f \
		-o $@


$(P)/jctic.o: jctic.f jcticcom
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/jcstring.o: jcstring.f
	$(FC) \
		$(M9INC) \
		$(PIC_FFLAGS) \
		-I$(PREFIX)/mod \
		-c $< \
		-o $@


$(P)/jcsys.o: $(P)/jcsys.f
	$(FC) \
		$(M9INC) \
		-I./ \
		-I$(P) \
		-I$(PREFIX)/mod \
		$(PIC_FFLAGS) \
		-c $< \
		-o $@


# ======================================================================
# Static libsg.a rules
# ======================================================================

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
		rm -f $(O)/$%; \
	fi


$(A)/libsg.a(sgvarg.o): sgvarg.c sgvarg_code.h
	@$(cmp_varg)
	@$(AR) $@ $(O)/$%
	@if test -z "$(DEBUG)"; then \
		rm -f $(O)/$%; \
	fi


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


# ======================================================================
# Static libjc.a rules
# ======================================================================

$(A)/libjc.a(jcmenu.o): jcmenu.f jcticcom
	@$(f_comp)


$(A)/libjc.a(jcprompt.o): jcprompt.F jcticcom
	@$(FPP) $(CPPFLAGS) $(SGDEFS) - < $< | \
		grep -v '^$$' > $(O)/$*.f
	@$(FC) -I./ $(FFLAGS) $(O)/$% $(O)/$*.f
	@$(AR) $@ $(O)/$%
	@if test -z "$(DEBUG)"; then \
		rm -f $(O)/$%; \
	fi


$(A)/libjc.a(jcstring.o): jcstring.f
	@$(f_comp)


$(A)/libjc.a(jctic.o): jctic.f jcticcom
	@$(f_comp)


$(A)/libjc.a(jcsys.o): $(O)/jcsys.f
	@$(f_proc)


# ======================================================================
# Existing install/executable rules
# ======================================================================

-include $(TRINS)Make_install


# ======================================================================
# Cleaning
# ======================================================================

cleanlib:
	@rm -f $(PREFIX)/lib/libsg.a
	@rm -f $(PREFIX)/lib/libjc.a


cleanshared:
	@rm -f $(PREFIX)/lib/libsg.so
	@rm -f $(PREFIX)/lib/libjc.so


cleanexe:
	@rm -f $(PREFIX)/bin/tstp10
	@rm -f $(PREFIX)/bin/tstcon
	@rm -f $(PREFIX)/bin/xplot
	@rm -f $(PREFIX)/bin/r4tstcon
	@rm -f $(PREFIX)/bin/r4tstp10
	@rm -f $(PREFIX)/bin/r8tstcon
	@rm -f $(PREFIX)/bin/r8tstp10


clean:
	@rm -f $(O)/*.o
	@rm -f $(O)/*.f
	@rm -f $(P)/*.o
	@rm -f $(P)/*.f
	@rm -f $(PREFIX)/include/*_inc
	@rm -f $(PREFIX)/mod/agcget_mod.mod
	@rm -f $(PREFIX)/mod/agcu_mod.mod
	@rm -f $(PREFIX)/mod/sgint_mod.mod
	@rm -f $(PREFIX)/mod/sglib_mod.mod
	@rm -f $(PREFIX)/mod/sgnco_mod.mod


clobber: clean cleanlib cleanshared cleanexe
