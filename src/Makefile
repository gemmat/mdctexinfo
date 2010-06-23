GOSH = /usr/local/bin/gosh
MAKEINFO  = makeinfo
TEXI2HTML = texi2html
TEXI2DVI  = texi2dvi
TEX = ptex

.PHONY: all
all: info html tangle

info:
	$(MAKEINFO) src.texi

html:
	$(TEXI2HTML) src.texi

tangle:
	$(GOSH) ./tangle.scm *.texi