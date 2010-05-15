GOSH = /usr/local/bin/gosh
MAKEINFO  = makeinfo
TEXI2HTML = texi2html
TEXI2DVI  = texi2dvi
TEX = ptex
OFFLINE_SCM   = offline.scm
MERGELANG_SCM = mergelang.scm
DOCORDER_SCM  = docorder.scm
INCLUDES_SCM  = includes.scm
XML2TEXI_SCM  = xml2texi.scm
UTF8_CONV_SCM = utf8conv.scm

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
	$(GOSH) $(INCLUDES_SCM) | xargs $(GOSH) $(XML2TEXI_SCM) --order=./order.scm --notfound=./notfound.scm -v

xml2texi-debug:
	$(GOSH) $(XML2TEXI_SCM) -v --debug

includes :
	$(GOSH) $(INCLUDES_SCM) -t ./order.scm > includes.texi

info:
	$(MAKEINFO) --error-limit=3000 ultima.texi 2> err

htmls:
	$(TEXI2HTML) --init-file ja-init.pl --split=section ultima.texi

euc_jp_texi:
	find includes.texi ultima.texi texi/ -name "*.texi" | xargs $(GOSH) $(UTF8_CONV_SCM) --encoding=euc_jp
	@echo "Successfully converted character encodings from utf8 to euc_jp."
	@echo "Please edit euc_jp_ultima.texi to write @documentencoding euc-jp and @includes euc_jp_includes.texi"
	@echo "Please edit euc_jp_includes.texi to replace @includes file paths"

sjis_texi:
	find includes.texi ultima.texi texi/ -name "*.texi" | xargs $(GOSH) $(UTF8_CONV_SCM) --encoding=sjis
	@echo "Successfully converted character encodings from utf8 to euc_jp."
	@echo "Please edit sjis_ultima.texi to write @documentencoding sjis and @includes sjis_includes.texi"
	@echo "Please edit sjis_includes.texi to replace @includes file paths"

dvi:
	TEX=$(TEX) $(TEXI2DVI) -t "@afourpaper" euc_jp_ultima.texi

pdf: dvi
	dvipdfmx euc_jp_ultima.dvi

chm:
	$(TEXI2HTML) --init-file chm.init sjis_ultima.texi

.PHONY: clean
clean:
	rm -rf out/developer.mozilla.org/en/
	rm -rf out/developer.mozilla.org/ja/
	rm -rf texi/
	rm -f  ultima.info*

clean-merge:
	find out/developer.mozilla.org/ja -name "*.html" -print0 | xargs -0 fgrep -l "not_yet_translated" | xargs rm
