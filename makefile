
# ---------------------------------------------------------------------------
# Makefile to build all UFILES libraries and executables
#
# Supports:
#   - Static libraries
#   - Shared libraries (MAKE_SO=1)
#   - Sequential library dependency builds
#   - Parallel compilation within individual libraries
#
# IMPORTANT:
#
#   PREFIX is converted to an absolute path and exported to all
#   recursive Make processes.
#
#   This ensures that ufport, sglib, and all remaining libraries
#   use the same build directory.
#
# Build order:
#
#       check
#         |
#       ufport
#         |
#       verify_ufport
#         |
#       sglib
#         |
#       remaining libraries
#         |
#       executables
# ---------------------------------------------------------------------------

SHELL := /bin/bash

.DEFAULT_GOAL := all


# ---------------------------------------------------------------------------
# Build configuration
# ---------------------------------------------------------------------------

NTASKS ?= 1

MAKE_SO ?= 0


# ---------------------------------------------------------------------------
# Absolute source and build directories
#
# PREFIX may be defined by the sourced environ script as:
#
#     builds/gcc/13.2.0
#
# Relative paths cause recursive Make processes to create build
# directories inside individual source directories.
#
# Convert PREFIX to an absolute path relative to the top-level
# UFILES directory before invoking recursive Make processes.
#
# The override directive also handles PREFIX supplied on the
# make command line.
# ---------------------------------------------------------------------------

override UFILES_HOME := $(abspath $(CURDIR))

ifeq ($(strip $(PREFIX)),)

override PREFIX := $(UFILES_HOME)/builds

else

override PREFIX := $(abspath $(PREFIX))

endif

export UFILES_HOME
export PREFIX
export MAKE_SO


# ---------------------------------------------------------------------------
# Build directories
# ---------------------------------------------------------------------------

BINDIR := $(PREFIX)/bin
INCDIR := $(PREFIX)/include
LIBDIR := $(PREFIX)/lib
MODDIR := $(PREFIX)/mod
MPIMODDIR := $(PREFIX)/mpi_mod
OBJDIR := $(PREFIX)/obj

UFPORT_SO := $(LIBDIR)/libufport.so
UFPORT_AR := $(LIBDIR)/libufport.a


# ---------------------------------------------------------------------------
# Libraries
#
# IMPORTANT:
#
#   ufport is built separately before sglib.
#
#   sglib is also built separately because libsg/libjc have a
#   special circular shared-library dependency handled inside
#   the sglib Makefile.
#
#   Libraries below are ordered so that every shared-library
#   dependency is built before the library that uses it.
# ---------------------------------------------------------------------------

LIBS := \
	sgdummy \
	mds_dummy \
	tokid \
	r4smlib \
	ufencod \
	ufhdf \
	ureadsub \
	mdstransp \
	mds_sub \
	uflib \
	trgraf \
	rngsel_sub \
	uflib_userio \
	r4smlib_userio \
	gdiff1sub \
	interp_sub \
	concat_sub \
	extrac_sub \
	sawcor_sub \
	splice_sub \
	thinsub


# ---------------------------------------------------------------------------
# Executables
# ---------------------------------------------------------------------------

EXES := \
	cmp_2d \
	concat \
	extrac \
	find_yg \
	fooch \
	gaver0 \
	gaver1 \
	gaver2 \
	gdiff1 \
	gsmoo1 \
	gsmoo2 \
	gxint1 \
	map2 \
	mds_get_inf \
	mdsmark \
	mds_start \
	mult2 \
	nulltron \
	propane \
	repl1 \
	repl2 \
	sawtoo \
	splice \
	splice2 \
	splice3 \
	test3d \
	tgtest \
	thin1 \
	thin2 \
	ucheck \
	uftype \
	ugraf1 \
	ugraf2 \
	ugraf3 \
	ulook0 \
	wiggle


# ---------------------------------------------------------------------------
# Phony targets
# ---------------------------------------------------------------------------

.PHONY: \
	all \
	check \
	show-config \
	ufport_lib \
	verify_ufport \
	sglib \
	libs \
	exes \
	clean \
	clean_sglib_mod \
	realclean \
	clobber \
	depend


# ---------------------------------------------------------------------------
# Main build
# ---------------------------------------------------------------------------

all: exes


# ---------------------------------------------------------------------------
# Show build configuration
# ---------------------------------------------------------------------------

show-config:
	@echo "============================================================"
	@echo "UFILES build configuration"
	@echo "============================================================"
	@echo "UFILES_HOME = $(UFILES_HOME)"
	@echo "PREFIX      = $(PREFIX)"
	@echo "MAKE_SO     = $(MAKE_SO)"
	@echo "NTASKS      = $(NTASKS)"
	@echo "LIBDIR      = $(LIBDIR)"
	@echo "OBJDIR      = $(OBJDIR)"
	@echo "============================================================"


# ---------------------------------------------------------------------------
# Create build directories
#
# All directories are created under the absolute PREFIX.
# ---------------------------------------------------------------------------

check:
	@mkdir -p "$(BINDIR)"
	@mkdir -p "$(INCDIR)"
	@mkdir -p "$(LIBDIR)"
	@mkdir -p "$(MODDIR)"
	@mkdir -p "$(MPIMODDIR)"
	@mkdir -p "$(OBJDIR)"

	@mkdir -p "$(OBJDIR)/ufport"
	@mkdir -p "$(OBJDIR)/sglib"
	@mkdir -p "$(OBJDIR)/sglib_pic"

	@set -e; \
	for l in $(LIBS); do \
		mkdir -p "$(OBJDIR)/$${l}"; \
	done

	@set -e; \
	for e in $(EXES); do \
		mkdir -p "$(OBJDIR)/$${e}"; \
	done

	@$(MAKE) -C ufport check

	@set -e; \
	for l in $(LIBS); do \
		$(MAKE) -C "$${l}" check; \
	done

	@set -e; \
	for e in $(EXES); do \
		$(MAKE) -C "$${e}" check; \
	done


# ---------------------------------------------------------------------------
# Build UFPORT
#
# UFPORT must be completely built before SGLIB.
#
# MAKE_SO=1 is propagated automatically.
# ---------------------------------------------------------------------------

ufport_lib: check
	@echo "============================================================"
	@echo "Building ufport"
	@echo "============================================================"

	@$(MAKE) -C ufport -j $(NTASKS) ufport


# ---------------------------------------------------------------------------
# Verify UFPORT
#
# Confirm that the required library exists in the common build
# directory before SGLIB starts.
#
# For MAKE_SO=1, require libufport.so.
# ---------------------------------------------------------------------------

verify_ufport: ufport_lib
	@echo "============================================================"
	@echo "Checking UFPORT libraries"
	@echo "============================================================"

	@echo "PREFIX  = $(PREFIX)"
	@echo "MAKE_SO = $(MAKE_SO)"

	@ls -l "$(LIBDIR)"/libufport.* 2>/dev/null || true

ifeq ($(MAKE_SO),1)

	@test -f "$(UFPORT_SO)" || { \
		echo "ERROR: Shared-library build requested."; \
		echo "Missing library: $(UFPORT_SO)"; \
		echo "Check ufport Makefile and PREFIX configuration."; \
		exit 1; \
	}

else

	@test -f "$(UFPORT_AR)" -o -f "$(UFPORT_SO)" || { \
		echo "ERROR: Neither libufport.a nor libufport.so was found."; \
		echo "Expected directory: $(LIBDIR)"; \
		exit 1; \
	}

endif


# ---------------------------------------------------------------------------
# Build SGLIB
#
# SGLIB depends on UFPORT.
#
# Its Makefile handles the circular shared-library dependency
# between libsg and libjc.
# ---------------------------------------------------------------------------

sglib: verify_ufport
	@echo "============================================================"
	@echo "Building sglib"
	@echo "============================================================"

	@$(MAKE) -C sglib -j $(NTASKS) libs


# ---------------------------------------------------------------------------
# Build remaining libraries
#
# The loop is intentionally sequential because shared libraries
# depend on libraries appearing earlier in LIBS.
#
# Parallel compilation is allowed only inside each library.
# ---------------------------------------------------------------------------

libs: sglib
	@set -e; \
	for l in $(LIBS); do \
		echo "============================================================"; \
		echo "Building $${l}"; \
		echo "============================================================"; \
		$(MAKE) -C "$${l}" -j $(NTASKS) "$${l}"; \
	done


# ---------------------------------------------------------------------------
# Build executables
#
# Executables are built only after all libraries have completed.
# ---------------------------------------------------------------------------

exes: libs
	@set -e; \
	for e in $(EXES); do \
		echo "============================================================"; \
		echo "Building $${e}"; \
		echo "============================================================"; \
		$(MAKE) -C "$${e}" "$${e}"; \
	done


# ---------------------------------------------------------------------------
# Remove stale SGLIB source-directory Fortran modules
#
# GCC searches the current source directory for .mod files.
# A stale or incompatible module can cause errors such as:
#
#   f951: Fatal Error: Reading module 'sgnco_mod.mod':
#         Unexpected EOF
#
# Generated modules should be stored in $(MODDIR).
#
# This target removes only source-directory .mod files.
# ---------------------------------------------------------------------------

clean_sglib_mod:
	@echo "Removing stale SGLIB Fortran module files"
	@rm -f -- "$(UFILES_HOME)"/sglib/*.mod


# ---------------------------------------------------------------------------
# Clean object files and generated Fortran modules
# ---------------------------------------------------------------------------

clean:
	@$(MAKE) -C ufport clean
	@$(MAKE) -C sglib clean
	@$(MAKE) clean_sglib_mod

	@set -e; \
	for l in $(LIBS); do \
		$(MAKE) -C "$${l}" clean; \
	done

	@set -e; \
	for e in $(EXES); do \
		$(MAKE) -C "$${e}" clean; \
	done


# ---------------------------------------------------------------------------
# Real clean
#
# Remove objects, libraries, and executables.
#
# SGLIB uses clobber because its clean target may remove only
# intermediate objects.
# ---------------------------------------------------------------------------

realclean:
	@$(MAKE) -C ufport realclean
	@$(MAKE) -C sglib clobber
	@$(MAKE) clean_sglib_mod

	@set -e; \
	for l in $(LIBS); do \
		$(MAKE) -C "$${l}" realclean; \
	done

	@set -e; \
	for e in $(EXES); do \
		$(MAKE) -C "$${e}" realclean; \
	done


# ---------------------------------------------------------------------------
# Remove entire build tree
#
# Safety checks prevent accidental deletion when PREFIX is empty
# or resolves to the root directory.
#
# Also remove stale SGLIB source-directory modules.
# ---------------------------------------------------------------------------

clobber:
	@echo "============================================================"
	@echo "Removing UFILES build directory"
	@echo "PREFIX = $(PREFIX)"
	@echo "============================================================"

	@test -n "$(strip $(PREFIX))"
	@test "$(PREFIX)" != "/"
	@test "$(PREFIX)" != "$(UFILES_HOME)"

	@$(MAKE) clean_sglib_mod
	@rm -rf -- "$(PREFIX)"


# ---------------------------------------------------------------------------
# Generate dependencies
# ---------------------------------------------------------------------------

depend: check
	@$(MAKE) -C ufport depend

	@set -e; \
	for l in $(LIBS); do \
		$(MAKE) -C "$${l}" depend; \
	done
