MODULE   = extcheck
MAGICDIR = ..
SRCS     = extcheck.c

EXTRA_LIBS = ${MAGICDIR}/extflat/libextflat.o ${MAGICDIR}/utils/libutils.a

include ${MAGICDIR}/defs.mak

LIBS += -lm

main: extcheck

install: $(DESTDIR)${BINDIR}/${MODULE}${EXEEXT}

install-tcl:
	echo "Nothing to do here"

include ${MAGICDIR}/rules.mak
