# Makefile include file for PPPL Intel 2019
#

ifndef MAKE_SO
	MAKE_SO = 0
endif
ifndef DEBUG
	DEBUG = 0
endif

ifeq ($(MAKE_SO),1)
	FPIC := -fPIC
endif
ifeq ($(MAKE_SO),TRUE)
	FPIC := -fPIC
endif
FLUX_FLG = -axCORE-AVX512 -fp-model precise

AR = ar rcs

# Fortran flags
FFLAGS_OPT = -c -O -nowarn -ftz -traceback -align dcommons -heap-arrays 20 $(FPIC) $(FLUX_FLG)
FFLAGS_DBG = -c -g -w -check bounds -check pointer -check uninit -fpe0 -traceback -align dcommons -heap-arrays 20 $(FPIC)
ifeq ($(DEBUG),0)
	FFLAGS = $(FFLAGS_OPT)
else
	FFLAGS = $(FFLAGS_DBG)
endif

FPP = -fpp
MFLAG = -module

# C/C++ flags
CFLAGS_OPT = -c -g -O2 -w -mp $(FPIC) $(FLUX_FLG)
CFLAGS_DBG = -c -g -O0 -w -mp $(FPIC)
CXXFLAGS_OPT = -c -g -O2 -w -mp $(FPIC) $(FLUX_FLG)
CXXFLAGS_DBG = -c -g -O0 -w -mp $(FPIC)
ifeq ($(DEBUG),0)
	CFLAGS = $(CFLAGS_OPT)
	CXXFLAGS = $(CXXFLAGS_OPT)
else
	CFLAGS = $(CFLAGS_DBG)
	CXXFLAGS = $(CXXFLAGS_DBG)
endif

CPP = -cpp

L_CLIBS = -L/usr/lib64 -lc -lstdc++


# For readline etc
L_EDIT = -L$(FLIBROOT) -lifport -limf -lifcore -lirc -L/usr/lib64 -lreadline -lhistory -ltermcap


# Default include directories
FINCL :=
CINCL :=


# Default definitions
FDEFS := -D__LINUX -D__UNIX -D__F90
CDEFS := -D__LINUX

#FDEFS := $(FDEFS) -D__NOMDSPLUS
#CDEFS := $(CDEFS) -D__NOMDSPLUS
