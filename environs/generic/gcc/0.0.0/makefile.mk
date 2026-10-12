# ======================================================================
# Generic GCC compiler configuration
#
# Used by UFILES / TRANSP
#
# Supports GCC 10+
# Static and shared library builds
# ======================================================================

ifndef MAKE_SO
    MAKE_SO = 0
endif

ifndef DEBUG
    DEBUG = 0
endif

# ======================================================================
# Shared library flags
# ======================================================================

FPIC := -fPIC

# ======================================================================
# Archive tools
# ======================================================================

AR = ar rcs
RANLIB = ranlib

# ======================================================================
# GCC version
# ======================================================================

GCC_MAJOR := $(shell $(FC) -dumpversion 2>/dev/null | cut -d. -f1)

# ======================================================================
# Fortran flags
# ======================================================================

FFLAGS_OPT = \
    -c \
    -g \
    -O2 \
    -m64 \
    -ffpe-trap=invalid,zero,overflow \
    -fno-range-check \
    -fdollar-ok \
    $(FPIC)

FFLAGS_DBG = \
    -c \
    -g \
    -O0 \
    -m64 \
    -ffpe-trap=invalid,zero,overflow \
    -fno-range-check \
    -fdollar-ok \
    $(FPIC)

# Compatibility options for GCC 10 and newer.

ifeq ($(shell test "$(GCC_MAJOR)" -gt 9 2>/dev/null; echo $$?),0)
    FFLAGS_OPT += -fallow-argument-mismatch -fallow-invalid-boz
    FFLAGS_DBG += -fallow-argument-mismatch -fallow-invalid-boz
endif

ifeq ($(DEBUG),0)
    FFLAGS = $(FFLAGS_OPT)
else
    FFLAGS = $(FFLAGS_DBG)
endif

MPI_FFLAGS = $(FFLAGS)

# ======================================================================
# Fortran preprocessing and module flags
# ======================================================================

FPP = -cpp
MFLAG = -J

# ======================================================================
# Fortran runtime libraries
# ======================================================================

L_FLIBS = -L$(FLIBROOT)/lib -lgfortran -lm

# ======================================================================
# C compiler flags
# ======================================================================

CFLAGS_OPT = -c -g -O2 -m64 $(FPIC)
CFLAGS_DBG = -c -g -O0 -m64 $(FPIC)

ifeq ($(DEBUG),0)
    CFLAGS = $(CFLAGS_OPT)
else
    CFLAGS = $(CFLAGS_DBG)
endif

# ======================================================================
# C++ compiler flags
# ======================================================================

CXXFLAGS_OPT = -g -O2 -m64 $(FPIC)
CXXFLAGS_DBG = -g -O0 -m64 $(FPIC)

ifeq ($(DEBUG),0)
    CXXFLAGS = $(CXXFLAGS_OPT)
else
    CXXFLAGS = $(CXXFLAGS_DBG)
endif

# ======================================================================
# Preprocessing
# ======================================================================

CPP = -cpp

# ======================================================================
# C/C++ runtime libraries
# ======================================================================

L_CLIBS = -L$(FLIBROOT)/lib -lstdc++ -lgcc_s -lgcc

# ======================================================================
# Readline libraries
# ======================================================================

L_EDIT = -L/usr/lib64 -lreadline -lhistory -ltermcap

# ======================================================================
# Include directories
# ======================================================================

FINCL :=
CINCL :=

# ======================================================================
# Preprocessor definitions
# ======================================================================

FDEFS := \
    -D__LINUX \
    -D__UNIX \
    -D__F90 \
    -D__NOMDSPLUS

CDEFS := \
    -D__LINUX \
    -D__NOMDSPLUS

# ======================================================================
# End
# ======================================================================
