GOSH = /usr/local/bin/gosh
MAKEINFO = makeinfo
TEXI2HTML = texi2html
OFFLINE_SCM = /usr/local/src/mdctexinfo/offline.scm
MERGELANG_SCM = /usr/local/src/mdctexinfo/mergelang.scm
DOCORDER_SCM = /usr/local/src/mdctexinfo/docorder.scm
INCLUDES_SCM =/usr/local/src/mdctexinfo/includes.scm
XML2TEXI_SCM = /usr/local/src/mdctexinfo/xml2texi.scm

.PHONY: all
all: offline merge xml2texi includes.texi info

offline:
	find developer.mozilla.org -name "*.html" -print0 | xargs -0 -n 1000 -P 0 $(GOSH) $(OFFLINE_SCM) -v

offline-debug:
	find developer.mozilla.org -name "*.html" -print0 | xargs -0 -n 1000 -P 0 $(GOSH) $(OFFLINE_SCM) -v -d

merge:
	find out/developer.mozilla.org/en -name "*.html" -print0 | xargs -0 -n 1000 -P 0 $(GOSH) $(MERGELANG_SCM) -v

order:
	$(GOSH) $(DOCORDER_SCM) out/developer.mozilla.org/ja > order.scm

xml2texi:
	$(GOSH) $(INCLUDES_SCM) | xargs $(GOSH) $(XML2TEXI_SCM) -v

xml2texi-debug:
	$(GOSH) $(XML2TEXI_SCM) -v --debug

includes :
	$(GOSH) $(INCLUDES_SCM) -t > includes.texi

info:
	$(MAKEINFO) --error-limit=3000 ultima.texi 2> err

html:
	$(TEXI2HTML) ultima.texi

.PHONY: clean
clean:
	rm -rf out/developer.mozilla.org/en/
	rm -rf out/developer.mozilla.org/ja/
	rm -rf texi/

clean-merge:
	find out/developer.mozilla.org/ja -name "*.html" -print0 | xargs -0 fgrep -l "not_yet_translated" | xargs rm
