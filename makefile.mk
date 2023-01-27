# Makefile to build all libs and exes
#

# Libraries (order is important)
LIBS := ufport \
	ufhdf \
	extrac_sub \
	trgraf \
	mds_sub \
	mdstransp \
	tokid \
	ufencod \
	uflib \
	uflib_userio \
	ureadsub \
	mds_dummy \
	sgdummy

# Executables
EXES := cmp_2d \
	find_yg \
	fooch \
	map2 \
	mds_get_inf \
	mdsmark \
	mds_start \
	mult2 \
	nulltron \
	repl1 \
	repl2 \
	test3d \
	tgtest \
	ucheck \
	uftype \
	ugraf1 \
	ugraf2 \
	ugraf3 \
	ulook0

.PHONY: check clean realclean clobber libs exes sglib depend

all:	check sglib libs exes

check:
	@test -d $(PREFIX)/bin || mkdir -p $(PREFIX)/bin
	@test -d $(PREFIX)/include || mkdir -p $(PREFIX)/include
	@test -d $(PREFIX)/lib || mkdir -p $(PREFIX)/lib
	@test -d $(PREFIX)/mod || mkdir -p $(PREFIX)/mod
	@test -d $(PREFIX)/obj || mkdir -p $(PREFIX)/obj
	@test -d $(PREFIX)/other || mkdir -p $(PREFIX)/other
	@set -e; for l in $(LIBS); do cd $${l}; make -f $${l}_makefile.mk check; cd .. ; done
	@set -e; for e in $(EXES); do cd $${e}; make -f $${e}_makefile.mk check; cd .. ; done

sglib:
	@cd ufport; make -f ufport_makefile.mk -j $(NTASKS); cd ..
	@cd $(UFILES_HOME)/sglib ; make all ; cd $(UFILES_HOME)

libs:
	@set -e; for l in $(LIBS); do cd $${l}; make -f $${l}_makefile.mk -j $(NTASKS) $${l}; cd .. ; done

exes:
	@set -e; for e in $(EXES); do cd $${e}; make -f $${e}_makefile.mk $${e}; cd .. ; done

clean: 
	@set -e; for l in $(LIBS); do cd $${l}; make -f $${l}_makefile.mk clean; cd .. ; done
	@set -e; for e in $(EXES); do cd $${e}; make -f $${e}_makefile.mk clean; cd .. ; done

realclean:
	@set -e; for l in $(LIBS); do cd $${l}; make -f $${l}_makefile.mk realclean; cd .. ; done
	@set -e; for e in $(EXES); do cd $${e}; make -f $${e}_makefile.mk realclean; cd .. ; done

clobber:
	@rm -rf $(PREFIX)

depend:
	@set -e; for l in $(LIBS); do cd $${l}; make -f $${l}_makefile.mk depend; cd .. ; done
