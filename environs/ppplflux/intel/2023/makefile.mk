
# ---------------------------------------------------------------------------
# Makefile include file
#
# PPPL Flux
# Intel oneAPI 2023.2.0
# OpenMPI 4.1.6
#
# Compilers:
#   FC  = ifort
#   CC  = icx
#   CXX = icpx
#
# Intended for UFILES and TRANSP static/shared-library builds.
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Build options
#
# Command-line values such as:
#
#   make MAKE_SO=1 DEBUG=0
#
# take precedence over these defaults.
# ---------------------------------------------------------------------------

MAKE_SO ?= 0
DEBUG   ?= 0


# ---------------------------------------------------------------------------
# Compilers
#
# GNU Make has built-in defaults for FC, CC, and CXX.
# Override built-in defaults, but preserve explicit environment
# and command-line compiler selections.
# ---------------------------------------------------------------------------

ifeq ($(origin FC),default)
    FC = ifort
endif

ifeq ($(origin FC),undefined)
    FC = ifort
endif

ifeq ($(origin CC),default)
    CC = icx
endif

ifeq ($(origin CC),undefined)
    CC = icx
endif

ifeq ($(origin CXX),default)
    CXX = icpx
endif

ifeq ($(origin CXX),undefined)
    CXX = icpx
endif


# ---------------------------------------------------------------------------
# Position-independent code
#
# Required for shared libraries.
#
# Using -fPIC for both static and shared objects is safe.
# ---------------------------------------------------------------------------

ifeq ($(filter 1 TRUE true YES yes,$(MAKE_SO)),)
    FPIC :=
else
    FPIC := -fPIC
endif


# ---------------------------------------------------------------------------
# Floating-point behavior
#
# Intel oneAPI 2023:
#   -fp-model precise
#
# Avoid Intel architecture-specific optimization flags
# because Flux uses AMD processors.
# ---------------------------------------------------------------------------

override FLUX_FLG := -fp-model precise


# ---------------------------------------------------------------------------
# Archive utility
#
# Preserve legacy UFILES usage:
#
#   $(AR) libname.a objects...
#
# Do not change this to AR=ar unless every library
# Makefile explicitly supplies 'rcs'.
# ---------------------------------------------------------------------------

override AR := ar rcs


# ---------------------------------------------------------------------------
# Intel Fortran optimization flags
#
# Do not include -c here.
# The compilation rule must supply -c.
# ---------------------------------------------------------------------------

override FFLAGS_OPT := \
    -c \
    -g \
    -O2 \
    -traceback \
    -align dcommons \
    $(FPIC) \
    $(FLUX_FLG)


# ---------------------------------------------------------------------------
# Intel Fortran debug flags
# ---------------------------------------------------------------------------

override FFLAGS_DBG := \
    -c \
    -g \
    -O0 \
    -traceback \
    -check bounds \
    -check pointers \
    -check uninit \
    -fpe0 \
    -align dcommons \
    $(FPIC)


# ---------------------------------------------------------------------------
# Select Fortran flags
# ---------------------------------------------------------------------------

ifeq ($(DEBUG),0)
    override FFLAGS := $(FFLAGS_OPT)
else
    override FFLAGS := $(FFLAGS_DBG)
endif


# ---------------------------------------------------------------------------
# Fortran preprocessing and module output
# ---------------------------------------------------------------------------

override FPP := -fpp

override MFLAG := -module


# ---------------------------------------------------------------------------
# Intel C optimization flags
#
# icx uses the LLVM-based Intel compiler.
#
# Do not include -c here.
# ---------------------------------------------------------------------------

override CFLAGS_OPT := \
    -g \
    -O2 \
    $(FPIC) \
    $(FLUX_FLG)


# ---------------------------------------------------------------------------
# Intel C debug flags
# ---------------------------------------------------------------------------

override CFLAGS_DBG := \
    -g \
    -O0 \
    -Wall \
    -Wextra \
    $(FPIC)


# ---------------------------------------------------------------------------
# Intel C++ optimization flags
# ---------------------------------------------------------------------------

override CXXFLAGS_OPT := \
    -g \
    -O2 \
    $(FPIC) \
    $(FLUX_FLG)


# ---------------------------------------------------------------------------
# Intel C++ debug flags
# ---------------------------------------------------------------------------

override CXXFLAGS_DBG := \
    -g \
    -O0 \
    -Wall \
    -Wextra \
    $(FPIC)


# ---------------------------------------------------------------------------
# Select C/C++ flags
# ---------------------------------------------------------------------------

ifeq ($(DEBUG),0)

    override CFLAGS   := $(CFLAGS_OPT)
    override CXXFLAGS := $(CXXFLAGS_OPT)

else

    override CFLAGS   := $(CFLAGS_DBG)
    override CXXFLAGS := $(CXXFLAGS_DBG)

endif


# ---------------------------------------------------------------------------
# Preprocessor
#
# Preserve the legacy UFILES convention:
#   $(CPP) as a compiler flag.
#
# Note: Some Makefiles may instead expect CPP to be
# an executable command. Those recipes should be checked.
# ---------------------------------------------------------------------------

override CPP := -E


# ---------------------------------------------------------------------------
# Intel Fortran runtime
#
# FLIBROOT must be supplied by the Intel environment.
# ---------------------------------------------------------------------------

override L_FLIBS := \
    -L$(FLIBROOT) \
    -lifcore \
    -lifport \
    -limf \
    -lsvml \
    -lintlc \
    -lm


# ---------------------------------------------------------------------------
# C/C++ runtime libraries
# ---------------------------------------------------------------------------

override L_CLIBS := \
    -lstdc++ \
    -lm


# ---------------------------------------------------------------------------
# Readline and history
#
# Rocky Linux 9 uses ncurses/tinfo.
# ---------------------------------------------------------------------------

override L_EDIT := \
    -L$(FLIBROOT) \
    -lifport \
    -limf \
    -lifcore \
    -L/usr/lib64 \
    -lreadline \
    -lhistory \
    -ltinfo


# ---------------------------------------------------------------------------
# Default include directories
#
# Preserve include directories already supplied by the
# environment or parent Makefile.
# ---------------------------------------------------------------------------

ifeq ($(strip $(FINCL)),)
    FINCL :=
endif

ifeq ($(strip $(CINCL)),)
    CINCL :=
endif


# ---------------------------------------------------------------------------
# Default preprocessor definitions
#
# These are compiler-configuration settings.
# ---------------------------------------------------------------------------

override FDEFS := \
    -D__LINUX \
    -D__UNIX \
    -D__F90

override CDEFS := \
    -D__LINUX


# ---------------------------------------------------------------------------
# Optional MDSplus disable flags
#
# Uncomment if MDSplus support is disabled.
# ---------------------------------------------------------------------------

# override FDEFS += -D__NOMDSPLUS
# override CDEFS += -D__NOMDSPLUS


# ---------------------------------------------------------------------------
# End of Intel oneAPI 2023 makefile.mk
# ---------------------------------------------------------------------------
