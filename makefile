# ---------------------------------------------------------------------------
# Makefile to build all UFILES libraries and executables
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Libraries
#
# IMPORTANT:
#
#   ufport is built separately before sglib because sglib depends on it.
#
#   sglib is also built separately because libsg/libjc have a special
#   circular shared-library dependency handled inside the sglib Makefile.
#
#   Libraries below are ordered so that every shared-library dependency
#   is built before the library that uses it.
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
	ufport_lib \
	sglib \
	libs \
	exes \
	clean \
	realclean \
	clobber \
	depend


# ---------------------------------------------------------------------------
# Main build
#
# Explicit dependency chain:
#
#       check
#         |
#       ufport
#         |
#       sglib
#         |
#       remaining libraries
#         |
#       executables
#
# This prevents top-level "gmake -j" from building dependent libraries
# simultaneously.
# ---------------------------------------------------------------------------

all: exes


# ---------------------------------------------------------------------------
# Create build directories
# ---------------------------------------------------------------------------

check:
	@mkdir -p $(PREFIX)/bin
	@mkdir -p $(PREFIX)/include
	@mkdir -p $(PREFIX)/lib
	@mkdir -p $(PREFIX)/mod
	@mkdir -p $(PREFIX)/mpi_mod
	@mkdir -p $(PREFIX)/obj

	@mkdir -p $(PREFIX)/obj/ufport
	@mkdir -p $(PREFIX)/obj/sglib
	@mkdir -p $(PREFIX)/obj/sglib_pic

	@set -e; \
	for l in $(LIBS); do \
		mkdir -p $(PREFIX)/obj/$${l}; \
	done

	@set -e; \
	for e in $(EXES); do \
		mkdir -p $(PREFIX)/obj/$${e}; \
	done

	@cd ufport && $(MAKE) check

	@set -e; \
	for l in $(LIBS); do \
		cd $${l}; \
		$(MAKE) check; \
		cd ..; \
	done

	@set -e; \
	for e in $(EXES); do \
		cd $${e}; \
		$(MAKE) check; \
		cd ..; \
	done


# ---------------------------------------------------------------------------
# UFPORT
#
# Must be completely built before SGLIB starts.
#
# MAKE_SO=1 is propagated automatically by $(MAKE).
# ---------------------------------------------------------------------------

ufport_lib: check
	@echo "============================================================"
	@echo "Building ufport"
	@echo "============================================================"
	@cd ufport && $(MAKE) -j $(NTASKS) ufport


# ---------------------------------------------------------------------------
# SGLIB
#
# SGLIB requires libufport.
#
# Its Makefile handles the special libsg/libjc circular shared-library
# relationship and supports:
#
#       MAKE_SO=1
# ---------------------------------------------------------------------------

sglib: ufport_lib
	@echo "============================================================"
	@echo "Building sglib"
	@echo "============================================================"
	@cd $(UFILES_HOME)/sglib && $(MAKE) -j $(NTASKS) libs


# ---------------------------------------------------------------------------
# Remaining libraries
#
# The loop itself is intentionally sequential because shared libraries
# depend on libraries appearing earlier in LIBS.
#
# Parallelism is allowed only inside an individual library.
# ---------------------------------------------------------------------------

libs: sglib
	@set -e; \
	for l in $(LIBS); do \
		echo "============================================================"; \
		echo "Building $${l}"; \
		echo "============================================================"; \
		cd $${l}; \
		$(MAKE) -j $(NTASKS) $${l}; \
		cd ..; \
	done


# ---------------------------------------------------------------------------
# Executables
#
# Executables are built only after all libraries have completed.
# ---------------------------------------------------------------------------

exes: libs
	@set -e; \
	for e in $(EXES); do \
		echo "============================================================"; \
		echo "Building $${e}"; \
		echo "============================================================"; \
		cd $${e}; \
		$(MAKE) $${e}; \
		cd ..; \
	done


# ---------------------------------------------------------------------------
# Clean object files
# ---------------------------------------------------------------------------

clean:
	@cd ufport && $(MAKE) clean
	@cd $(UFILES_HOME)/sglib && $(MAKE) clean

	@set -e; \
	for l in $(LIBS); do \
		cd $${l}; \
		$(MAKE) clean; \
		cd ..; \
	done

	@set -e; \
	for e in $(EXES); do \
		cd $${e}; \
		$(MAKE) clean; \
		cd ..; \
	done


# ---------------------------------------------------------------------------
# Real clean
#
# Remove objects and built libraries/executables.
#
# SGLIB uses "clobber" here because its "clean" target removes only
# intermediate objects.  clobber also removes:
#
#       libsg.a
#       libjc.a
#       libsg.so
#       libjc.so
# ---------------------------------------------------------------------------

realclean:
	@cd ufport && $(MAKE) realclean
	@cd $(UFILES_HOME)/sglib && $(MAKE) clobber

	@set -e; \
	for l in $(LIBS); do \
		cd $${l}; \
		$(MAKE) realclean; \
		cd ..; \
	done

	@set -e; \
	for e in $(EXES); do \
		cd $${e}; \
		$(MAKE) realclean; \
		cd ..; \
	done


# ---------------------------------------------------------------------------
# Complete removal of build tree
# ---------------------------------------------------------------------------

clobber:
	@echo "Removing $(PREFIX)"
	@rm -rf $(PREFIX)


# ---------------------------------------------------------------------------
# Generate dependencies
# ---------------------------------------------------------------------------

depend: check
	@cd ufport && $(MAKE) depend

	@set -e; \
	for l in $(LIBS); do \
		cd $${l}; \
		$(MAKE) depend; \
		cd ..; \
	done
