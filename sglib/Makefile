# ======================================================================
# Makefile for SGLIB / JCLIB
#
# UFILES - Generic GNU/Linux
#
# Compiler configuration:
#     $(dir $(ENV_FILE))makefile.mk
#
# Static libraries:
#     libsg.a
#     libjc.a  (copy of libsg.a)
#
# Shared libraries (MAKE_SO=1):
#     libsg.so
#     libjc.so (copy of libsg.so)
#
# Both libraries contain ALL SGLIB and JCLIB objects.
#
# Usage:
#     make MAKE_SO=0 DEBUG=0 libs
#     make MAKE_SO=1 DEBUG=0 libs
#     make MAKE_SO=1 DEBUG=0 check
#     make clean
#     make clobber
#
# ======================================================================

SHELL := /bin/bash

.DEFAULT_GOAL := libs

# Disable built-in compilation rules.
MAKEFLAGS += -r
.SUFFIXES:

# Serialize builds because Fortran module dependencies are not
# fully represented by individual .mod prerequisites.
.NOTPARALLEL:

# ======================================================================
# Compiler configuration
# ======================================================================

# ENV_FILE must be exported by the UFILES environment.
# Do not reconstruct it from SITE, COMPILER_ENV or COMPILER_VER.

ifeq ($(strip $(ENV_FILE)),)
$(error ENV_FILE is not defined. Source the UFILES environment first.)
endif

COMPILER_MK := $(dir $(ENV_FILE))makefile.mk

ifeq ($(wildcard $(COMPILER_MK)),)
$(error Cannot find compiler configuration: $(COMPILER_MK); ENV_FILE=$(ENV_FILE))
endif

include $(COMPILER_MK)

# ======================================================================
# Build configuration
# ======================================================================

MAKE_SO ?= 0
DEBUG   ?= 0

# SHARED is a compatibility option.
# An explicit command-line MAKE_SO takes precedence.

ifneq ($(filter 1 y Y yes YES true TRUE,$(SHARED)),)
ifneq ($(origin MAKE_SO),command line)
MAKE_SO := 1
endif
endif

# ======================================================================
# Compiler and archive commands
# ======================================================================

# FC, CC, FFLAGS, CFLAGS, FDEFS, CDEFS, FINCL, CINCL,
# LDFLAGS and AR are supplied by makefile.mk.

ifeq ($(strip $(FC)),)
$(error FC is not defined in $(COMPILER_MK))
endif

ifeq ($(strip $(CC)),)
$(error CC is not defined in $(COMPILER_MK))
endif

# The generic GCC configuration defines:
#
#     AR = ar rcs
#
# Therefore archive recipes must use:
#
#     $(AR) archive.a objects...
#
# Do not append another "rcs".

RANLIB ?= ranlib

# ======================================================================
# Compiler flags
# ======================================================================

# Use the flags selected by makefile.mk.
#
# The compiler configuration already handles DEBUG and MAKE_SO.
# In particular, MAKE_SO=1 adds -fPIC through FPIC.

SG_FFLAGS := $(FFLAGS)
SG_CFLAGS := $(CFLAGS)

SG_FDEFS := $(FDEFS)
SG_CDEFS := $(CDEFS)

SG_FINCL := $(FINCL)
SG_CINCL := $(CINCL)

# Additional include directories required by SGLIB.

SG_FINCL += -I. -I$(PREFIX)/mod -I$(PREFIX)/obj/sglib
SG_CINCL += -I.

# ======================================================================
# Preprocessing
# ======================================================================

# FPP in the compiler configuration is a Fortran flag (-cpp),
# not a standalone preprocessing executable.
#
# Use the configured C compiler to preprocess legacy .F files.

SG_FPP_CMD := $(CC)

SG_FPPFLAGS := -E -P -x c -traditional

# Preserve the platform definitions from makefile.mk.
# __f90 is retained for compatibility with the legacy .F sources.

SG_FPPDEFS := $(FDEFS) -D__f90

# ======================================================================
# Directories
# ======================================================================

LIBDIR := $(PREFIX)/lib
OBJDIR := $(PREFIX)/obj/sglib
MODDIR := $(PREFIX)/mod

# ======================================================================
# Libraries
# ======================================================================

SGLIB_A := $(LIBDIR)/libsg.a
JCLIB_A := $(LIBDIR)/libjc.a

SGLIB_SO := $(LIBDIR)/libsg.so
JCLIB_SO := $(LIBDIR)/libjc.so

# ======================================================================
# SGLIB sources
#
# IMPORTANT:
#
# Linux uses sgsys.c.
# The legacy sgsys.f must not be compiled.
#
# ======================================================================

SG_F_SRC := \
    sgiso.f \
    sgarea.f \
    r8sgiso.f \
    r8sgext.f \
    r8sggrf.f \
    r8sgnco.f \
    r8sgtcs.f \
    r4sgext.f \
    r4sggrf.f \
    r4sgnco.f

SG_FPP_SRC := \
    sgnco.F \
    sgext.F \
    sggrf.F \
    sgtcs.F \
    sgiof.F

SG_C_SRC := \
    sgsys.c \
    sgvarg.c

# Fortran module sources.

SG_MOD_SRC := \
    agcget_mod.f \
    agcu_mod.f \
    sgint_mod.f \
    sglib_mod.f \
    sgnco_mod.f

# ======================================================================
# JCLIB sources
# ======================================================================

JC_F_SRC := \
    jcmenu.f \
    jcstring.f \
    jctic.f

JC_FPP_SRC := \
    jcprompt.F \
    jcsys.F

# ======================================================================
# Object lists
# ======================================================================

SG_MOD_OBJ := \
    $(patsubst %.f,$(OBJDIR)/%.o,$(SG_MOD_SRC))

SG_F_OBJ := \
    $(patsubst %.f,$(OBJDIR)/%.o,$(SG_F_SRC))

SG_FPP_OBJ := \
    $(patsubst %.F,$(OBJDIR)/%.o,$(SG_FPP_SRC))

SG_C_OBJ := \
    $(patsubst %.c,$(OBJDIR)/%.o,$(SG_C_SRC))

SG_OBJ := \
    $(SG_MOD_OBJ) \
    $(SG_F_OBJ) \
    $(SG_FPP_OBJ) \
    $(SG_C_OBJ)

JC_F_OBJ := \
    $(patsubst %.f,$(OBJDIR)/%.o,$(JC_F_SRC))

JC_FPP_OBJ := \
    $(patsubst %.F,$(OBJDIR)/%.o,$(JC_FPP_SRC))

JC_OBJ := \
    $(JC_F_OBJ) \
    $(JC_FPP_OBJ)

# ======================================================================
# Combined SGLIB / JCLIB objects
#
# Both libsg and libjc contain the same complete implementation.
# ======================================================================

ALL_SG_OBJ := $(SG_OBJ) $(JC_OBJ)

# ======================================================================
# Targets
# ======================================================================

.PHONY: \
    all \
    libs \
    static \
    shared \
    dirs \
    check \
    clean \
    cleanlib \
    cleanshared \
    clobber

all: libs

ifeq ($(MAKE_SO),1)
libs: static shared
else
libs: static
endif

static: $(SGLIB_A) $(JCLIB_A)

shared: $(SGLIB_SO) $(JCLIB_SO)

# ======================================================================
# Directories
# ======================================================================

$(LIBDIR) $(OBJDIR) $(MODDIR):
	@mkdir -p $@

dirs: $(LIBDIR) $(OBJDIR) $(MODDIR)

# ======================================================================
# Fortran module compilation
# ======================================================================

$(OBJDIR)/%_mod.o: %_mod.f | $(OBJDIR) $(MODDIR)
	@echo "Compiling Fortran module $<"
	$(FC) $(SG_FFLAGS) $(SG_FDEFS) $(SG_FINCL) \
	    -J$(MODDIR) \
	    -c $< -o $@

# ======================================================================
# Ordinary Fortran compilation
# ======================================================================

# Ensure modules are built before other Fortran objects.

$(SG_F_OBJ) $(JC_F_OBJ): $(SG_MOD_OBJ)

$(OBJDIR)/%.o: %.f | $(OBJDIR) $(MODDIR)
	@echo "Compiling Fortran $<"
	$(FC) $(SG_FFLAGS) $(SG_FDEFS) $(SG_FINCL) \
	    -J$(MODDIR) \
	    -c $< -o $@

# ======================================================================
# Preprocessing .F sources
# ======================================================================

# Preprocessed .f files are generated in OBJDIR.
# This avoids overwriting source files.

$(OBJDIR)/%.f: %.F | $(OBJDIR)
	@echo "Preprocessing $<"
	$(SG_FPP_CMD) $(SG_FPPFLAGS) \
	    $(CPPFLAGS) $(SG_FPPDEFS) \
	    -I. $< > $@

# ======================================================================
# Compile preprocessed Fortran
# ======================================================================

$(SG_FPP_OBJ) $(JC_FPP_OBJ): $(SG_MOD_OBJ)

$(OBJDIR)/%.o: $(OBJDIR)/%.f | $(OBJDIR) $(MODDIR)
	@echo "Compiling preprocessed Fortran $<"
	$(FC) $(SG_FFLAGS) $(SG_FDEFS) $(SG_FINCL) \
	    -J$(MODDIR) \
	    -c $< -o $@

# ======================================================================
# C compilation
# ======================================================================

# Explicit Linux SGSYS implementation.
# This prevents selection of sgsys.f.

$(OBJDIR)/sgsys.o: sgsys.c sgbuff.h | $(OBJDIR)
	@echo "Compiling Linux SGSYS: sgsys.c"
	$(CC) $(SG_CFLAGS) $(SG_CDEFS) $(SG_CINCL) \
	    -c $< -o $@

$(OBJDIR)/sgvarg.o: sgvarg.c sgvarg_code.h | $(OBJDIR)
	@echo "Compiling SGVARG: sgvarg.c"
	$(CC) $(SG_CFLAGS) $(SG_CDEFS) $(SG_CINCL) \
	    -c $< -o $@

# Generic C rule.

$(OBJDIR)/%.o: %.c | $(OBJDIR)
	@echo "Compiling C $<"
	$(CC) $(SG_CFLAGS) $(SG_CDEFS) $(SG_CINCL) \
	    -c $< -o $@

# ======================================================================
# Static libraries
#
# Both archives contain all SGLIB and JCLIB objects.
#
# libjc.a is an actual copy of libsg.a.
# ======================================================================

$(SGLIB_A): $(ALL_SG_OBJ) | $(LIBDIR)
	@echo "=================================================="
	@echo "Building combined $(SGLIB_A)"
	@echo "=================================================="
	@rm -f $@
	$(AR) $@ $(ALL_SG_OBJ)
	$(RANLIB) $@

$(JCLIB_A): $(SGLIB_A) | $(LIBDIR)
	@echo "=================================================="
	@echo "Copying $(SGLIB_A) to $(JCLIB_A)"
	@echo "=================================================="
	cp -f $(SGLIB_A) $(JCLIB_A)

# ======================================================================
# Shared libraries
#
# libsg.so contains all SGLIB and JCLIB objects.
# libjc.so is an actual copy of libsg.so.
#
# No circular dependencies.
# ======================================================================

SOFLAGS ?= -shared

SHARED_RPATH := -Wl,-rpath,'$$ORIGIN'

$(SGLIB_SO): $(ALL_SG_OBJ) | $(LIBDIR)
	@echo "=================================================="
	@echo "Building combined $(SGLIB_SO)"
	@echo "=================================================="
	$(FC) $(SOFLAGS) $(LDFLAGS) \
	    -Wl,-soname,libsg.so \
	    -o $@ \
	    $(ALL_SG_OBJ) \
	    -L$(LIBDIR) \
	    -lufport \
	    $(SHARED_RPATH)

$(JCLIB_SO): $(SGLIB_SO) | $(LIBDIR)
	@echo "=================================================="
	@echo "Copying $(SGLIB_SO) to $(JCLIB_SO)"
	@echo "=================================================="
	cp -f $(SGLIB_SO) $(JCLIB_SO)

# ======================================================================
# Diagnostics
# ======================================================================

check:
	@echo "=================================================="
	@echo "SGLIB / JCLIB compiler configuration"
	@echo "=================================================="
	@echo "ENV_FILE      = $(ENV_FILE)"
	@echo "COMPILER_MK   = $(COMPILER_MK)"
	@echo "PREFIX        = $(PREFIX)"
	@echo "FC            = $(FC)"
	@echo "CC            = $(CC)"
	@echo "AR            = $(AR)"
	@echo "RANLIB        = $(RANLIB)"
	@echo "FFLAGS        = $(FFLAGS)"
	@echo "CFLAGS        = $(CFLAGS)"
	@echo "FDEFS         = $(FDEFS)"
	@echo "CDEFS         = $(CDEFS)"
	@echo "FINCL         = $(FINCL)"
	@echo "CINCL         = $(CINCL)"
	@echo "LDFLAGS       = $(LDFLAGS)"
	@echo "SG_FFLAGS     = $(SG_FFLAGS)"
	@echo "SG_CFLAGS     = $(SG_CFLAGS)"
	@echo "MAKE_SO       = $(MAKE_SO)"
	@echo "DEBUG         = $(DEBUG)"
	@echo "LIBDIR        = $(LIBDIR)"
	@echo "OBJDIR        = $(OBJDIR)"
	@echo "MODDIR        = $(MODDIR)"
	@echo "=================================================="

# ======================================================================
# Clean
# ======================================================================

clean:
	@echo "Cleaning SGLIB/JCLIB objects"
	@rm -rf $(OBJDIR)
	@rm -f $(MODDIR)/agcget_mod.mod
	@rm -f $(MODDIR)/agcu_mod.mod
	@rm -f $(MODDIR)/sgint_mod.mod
	@rm -f $(MODDIR)/sglib_mod.mod
	@rm -f $(MODDIR)/sgnco_mod.mod

cleanlib:
	@echo "Removing static libraries"
	@rm -f $(SGLIB_A) $(JCLIB_A)

cleanshared:
	@echo "Removing shared libraries"
	@rm -f $(SGLIB_SO) $(JCLIB_SO)

clobber: clean cleanlib cleanshared

# ======================================================================
# End
# ======================================================================
