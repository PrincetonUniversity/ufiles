# Makefile to build all libs and exes
#

# Libraries (order is important)
LIBS := ufport \
	ufhdf \
	concat_sub \
	extrac_sub \
	gdiff1sub \
	interp_sub \
	trgraf \
	mds_sub \
	mdstransp \
	r4smlib \
	r4smlib_userio \
	rngsel_sub \
	tokid \
	ufencod \
	uflib \
	uflib_userio \
	ureadsub \
	mds_dummy \
	sawcor_sub \
	sgdummy \
	splice_sub \
	thinsub

# Executables
EXES := cmp_2d \
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

.PHONY: check clean realclean clobber libs exes sglib depend

all:	check sglib libs exes

check:
	@test -d $(PREFIX)/bin || mkdir -p $(PREFIX)/bin
	@test -d $(PREFIX)/include || mkdir -p $(PREFIX)/include
	@test -d $(PREFIX)/lib || mkdir -p $(PREFIX)/lib
	@test -d $(PREFIX)/mod || mkdir -p $(PREFIX)/mod
	@test -d $(PREFIX)/obj || mkdir -p $(PREFIX)/obj
	@set -e; for l in $(LIBS); do cd $${l}; make check; cd .. ; done
	@set -e; for e in $(EXES); do cd $${e}; make check; cd .. ; done

sglib:
	@cd ufport; make -j $(NTASKS); cd ..
	@cd $(UFILES_HOME)/sglib ; make all ; cd $(UFILES_HOME)

libs:
	@set -e; for l in $(LIBS); do cd $${l}; make -j $(NTASKS) $${l}; cd .. ; done

exes:
	@set -e; for e in $(EXES); do cd $${e}; make $${e}; cd .. ; done

clean: 
	@set -e; for l in $(LIBS); do cd $${l}; make clean; cd .. ; done
	@set -e; for e in $(EXES); do cd $${e}; make clean; cd .. ; done

realclean:
	@set -e; for l in $(LIBS); do cd $${l}; make realclean; cd .. ; done
	@set -e; for e in $(EXES); do cd $${e}; make realclean; cd .. ; done

clobber:
	@rm -rf $(PREFIX)

depend:
	@set -e; for l in $(LIBS); do cd $${l}; make depend; cd .. ; done
