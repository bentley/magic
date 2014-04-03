MODULE   = net2ir
MAGICDIR = ..
SRCS     = net2ir.c

EXTRA_LIBS  = ${MAGICDIR}/utils/libutils.a

include ${MAGICDIR}/defs.mak

tcl-main:
	echo "Nothing to do here"

main: net2ir

install: $(DESTDIR)${BINDIR}/${MODULE}${EXEEXT}

install-tcl:
	echo "Nothing to do here"

include ${MAGICDIR}/rules.mak
