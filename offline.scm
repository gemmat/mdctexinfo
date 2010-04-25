(use srfi-1)
(use srfi-13)
(use file.util)
(use rfc.uri)
(use gauche.parseopt)
(use sxml.sxpath)
(use sxml.ssax)
(use sxml.tools)
(use sxml.serializer)

(define xhtml-entity
  '((nbsp . "\u00a0")
    (iexcl . "\u00a1")
    (cent . "\u00a2")
    (pound . "\u00a3")
    (curren . "\u00a4")
    (yen . "\u00a5")
    (brvbar . "\u00a6")
    (sect . "\u00a7")
    (uml . "\u00a8")
    (copy . "\u00a9")
    (ordf . "\u00aa")
    (laquo . "\u00ab")
    (not . "\u00ac")
    (shy . "\u00ad")
    (reg . "\u00ae")
    (macr . "\u00af")
    (deg . "\u00b0")
    (plusmn . "\u00b1")
    (sup2 . "\u00b2")
    (sup3 . "\u00b3")
    (acute . "\u00b4")
    (micro . "\u00b5")
    (para . "\u00b6")
    (middot . "\u00b7")
    (cedil . "\u00b8")
    (sup1 . "\u00b9")
    (ordm . "\u00ba")
    (raquo . "\u00bb")
    (frac14 . "\u00bc")
    (frac12 . "\u00bd")
    (frac34 . "\u00be")
    (iquest . "\u00bf")
    (Agrave . "\u00c0")
    (Aacute . "\u00c1")
    (Acirc . "\u00c2")
    (Atilde . "\u00c3")
    (Auml . "\u00c4")
    (Aring . "\u00c5")
    (AElig . "\u00c6")
    (Ccedil . "\u00c7")
    (Egrave . "\u00c8")
    (Eacute . "\u00c9")
    (Ecirc . "\u00ca")
    (Euml . "\u00cb")
    (Igrave . "\u00cc")
    (Iacute . "\u00cd")
    (Icirc . "\u00ce")
    (Iuml . "\u00cf")
    (ETH . "\u00d0")
    (Ntilde . "\u00d1")
    (Ograve . "\u00d2")
    (Oacute . "\u00d3")
    (Ocirc . "\u00d4")
    (Otilde . "\u00d5")
    (Ouml . "\u00d6")
    (times . "\u00d7")
    (Oslash . "\u00d8")
    (Ugrave . "\u00d9")
    (Uacute . "\u00da")
    (Ucirc . "\u00db")
    (Uuml . "\u00dc")
    (Yacute . "\u00dd")
    (THORN . "\u00de")
    (szlig . "\u00df")
    (agrave . "\u00e0")
    (aacute . "\u00e1")
    (acirc . "\u00e2")
    (atilde . "\u00e3")
    (auml . "\u00e4")
    (aring . "\u00e5")
    (aelig . "\u00e6")
    (ccedil . "\u00e7")
    (egrave . "\u00e8")
    (eacute . "\u00e9")
    (ecirc . "\u00ea")
    (euml . "\u00eb")
    (igrave . "\u00ec")
    (iacute . "\u00ed")
    (icirc . "\u00ee")
    (iuml . "\u00ef")
    (eth . "\u00f0")
    (ntilde . "\u00f1")
    (ograve . "\u00f2")
    (oacute . "\u00f3")
    (ocirc . "\u00f4")
    (otilde . "\u00f5")
    (ouml . "\u00f6")
    (divide . "\u00f7")
    (oslash . "\u00f8")
    (ugrave . "\u00f9")
    (uacute . "\u00fa")
    (ucirc . "\u00fb")
    (uuml . "\u00fc")
    (yacute . "\u00fd")
    (thorn . "\u00fe")
    (yuml . "\u00ff")
    (OElig . "\u0152")
    (oelig . "\u0153")
    (Scaron . "\u0160")
    (scaron . "\u0161")
    (Yuml . "\u0178")
    (circ . "\u02c6")
    (tilde . "\u02dc")
    (ensp . "\u2002")
    (emsp . "\u2003")
    (thinsp . "\u2009")
    (zwnj . "\u200c")
    (zwj . "\u200d")
    (lrm . "\u200e")
    (rlm . "\u200f")
    (ndash . "\u2013")
    (mdash . "\u2014")
    (lsquo . "\u2018")
    (rsquo . "\u2019")
    (sbquo . "\u201a")
    (ldquo . "\u201c")
    (rdquo . "\u201d")
    (bdquo . "\u201e")
    (dagger . "\u2020")
    (Dagger . "\u2021")
    (permil . "\u2030")
    (lsaquo . "\u2039")
    (rsaquo . "\u203a")
    (euro . "\u20ac")
    (fnof . "\u0192")
    (Alpha . "\u0391")
    (Beta . "\u0392")
    (Gamma . "\u0393")
    (Delta . "\u0394")
    (Epsilon . "\u0395")
    (Zeta . "\u0396")
    (Eta . "\u0397")
    (Theta . "\u0398")
    (Iota . "\u0399")
    (Kappa . "\u039a")
    (Lambda . "\u039b")
    (Mu . "\u039c")
    (Nu . "\u039d")
    (Xi . "\u039e")
    (Omicron . "\u039f")
    (Pi . "\u03a0")
    (Rho . "\u03a1")
    (Sigma . "\u03a3")
    (Tau . "\u03a4")
    (Upsilon . "\u03a5")
    (Phi . "\u03a6")
    (Chi . "\u03a7")
    (Psi . "\u03a8")
    (Omega . "\u03a9")
    (alpha . "\u03b1")
    (beta . "\u03b2")
    (gamma . "\u03b3")
    (delta . "\u03b4")
    (epsilon . "\u03b5")
    (zeta . "\u03b6")
    (eta . "\u03b7")
    (theta . "\u03b8")
    (iota . "\u03b9")
    (kappa . "\u03ba")
    (lambda . "\u03bb")
    (mu . "\u03bc")
    (nu . "\u03bd")
    (xi . "\u03be")
    (omicron . "\u03bf")
    (pi . "\u03c0")
    (rho . "\u03c1")
    (sigmaf . "\u03c2")
    (sigma . "\u03c3")
    (tau . "\u03c4")
    (upsilon . "\u03c5")
    (phi . "\u03c6")
    (chi . "\u03c7")
    (psi . "\u03c8")
    (omega . "\u03c9")
    (thetasym . "\u03d1")
    (upsih . "\u03d2")
    (piv . "\u03d6")
    (bull . "\u2022")
    (hellip . "\u2026")
    (prime . "\u2032")
    (Prime . "\u2033")
    (oline . "\u203e")
    (frasl . "\u2044")
    (weierp . "\u2118")
    (image . "\u2111")
    (real . "\u211c")
    (trade . "\u2122")
    (alefsym . "\u2135")
    (larr . "\u2190")
    (uarr . "\u2191")
    (rarr . "\u2192")
    (darr . "\u2193")
    (harr . "\u2194")
    (crarr . "\u21b5")
    (lArr . "\u21d0")
    (uArr . "\u21d1")
    (rArr . "\u21d2")
    (dArr . "\u21d3")
    (hArr . "\u21d4")
    (forall . "\u2200")
    (part . "\u2202")
    (exist . "\u2203")
    (empty . "\u2205")
    (nabla . "\u2207")
    (isin . "\u2208")
    (notin . "\u2209")
    (ni . "\u220b")
    (prod . "\u220f")
    (sum . "\u2211")
    (minus . "\u2212")
    (lowast . "\u2217")
    (radic . "\u221a")
    (prop . "\u221d")
    (infin . "\u221e")
    (ang . "\u2220")
    (and . "\u2227")
    (or . "\u2228")
    (cap . "\u2229")
    (cup . "\u222a")
    (int . "\u222b")
    (there4 . "\u2234")
    (sim . "\u223c")
    (cong . "\u2245")
    (asymp . "\u2248")
    (ne . "\u2260")
    (equiv . "\u2261")
    (le . "\u2264")
    (ge . "\u2265")
    (sub . "\u2282")
    (sup . "\u2283")
    (nsub . "\u2284")
    (sube . "\u2286")
    (supe . "\u2287")
    (oplus . "\u2295")
    (otimes . "\u2297")
    (perp . "\u22a5")
    (sdot . "\u22c5")
    (lceil . "\u2308")
    (rceil . "\u2309")
    (lfloor . "\u230a")
    (rfloor . "\u230b")
    (lang . "\u2329")
    (rang . "\u232a")
    (loz . "\u25ca")
    (spades . "\u2660")
    (clubs . "\u2663")
    (hearts . "\u2665")
    (diams . "\u2666")
    ))

(with-module sxml.ssax
  (set! ssax:predefined-parsed-entities
        `(,@(with-module user xhtml-entity)
          ,@ssax:predefined-parsed-entities)))

(define verbose  #f)
(define prefix   #f)

(define (remove-useless-elements! sxml)
  (for-each (lambda (obj)
              (sxml:change-name! obj 'useless)
              (sxml:change-content!  obj `())
              (sxml:change-attrlist! obj `()))
            (append-map (cut <> sxml)
             `(,(sxpath '(// (or@ xhtml:script xhtml:iframe)))
               ,(sxpath '(// (xhtml:link (@ type  (equal? "application/rss+xml")))))
               ,(sxpath '(// (xhtml:ul   (@ id    (equal? "nav-access")))))
               ,(sxpath '(// (xhtml:ul   (@ id    (equal? "sitetools")))))
               ,(sxpath '(// (xhtml:ul   (@ class (equal? "page-anchors")))))
               ,(sxpath '(// (xhtml:div  (@ id    (equal? "deki-page-alerts")))))
               ,(sxpath '(// (xhtml:div  (@ id    (equal? "popupMessage")))))
               ,(sxpath '(// (xhtml:div  (@ id    (equal? "popupMask")))))
               ,(sxpath '(// (xhtml:div  (@ id    (equal? "popupContainer")))))
               ,(sxpath '(// (xhtml:div  (@ id    (equal? "page-tags")))))
               ,(sxpath '(// (xhtml:div  (@ id    (equal? "page-files")))))
               ,(sxpath '(// (xhtml:div  (@ id    (equal? "MTMessage")))))
               ,(sxpath '(// (xhtml:div  (@ class (equal? "siteNav")))))
               ,(sxpath '(// (xhtml:div  (@ class (equal? "siteSearch")))))
               ,(sxpath '(// (xhtml:div  (@ class (equal? "pageBar")))))
               ,(sxpath '(// (xhtml:div  (@ class (equal? "suggestchannels")))))))))

(define (expand-div! sxml)
  (for-each (lambda (obj)
              (when (and (string=? "" (sxml:string-value obj))
                         (zero? (length (sxml:child-elements obj))))
                (sxml:change-content! obj '(""))))
            ((sxpath '(// xhtml:div)) sxml)))

(define (resolve-uri base uri)
  (receive (scheme _ host _ path query fragment) (uri-parse uri)
    (and-let* ((path)
               (path (regexp-replace-all #/%3a/ path "/"))
               (rslv-path (if (relative-path? path)
                            (simplify-path (build-path base path))
                            path)))
      (uri-compose
       :scheme   (or scheme "https")
       :host     (or host "developer.mozilla.org")
       :path     rslv-path
       :query    query
       :fragment fragment))))

(define (process-links! base sxml)
  (define (append-extension path)
    (if (path-extension path)
      path
      (path-swap-extension path "html")))
  (define myhost (build-path prefix "developer.mozilla.org"))
  (for-each (lambda (obj)
              (and-let* ((rslv-uri (resolve-uri base (sxml:string-value obj))))
                (receive (scheme _ host _ path query fragment) (uri-parse rslv-uri)
                  (and-let* (((and scheme host))
                             ((string=? scheme "https"))
                             ((string=? host   "developer.mozilla.org")))
                    (sxml:change-content!
                     obj
                     `(,(uri-compose
                         :scheme   "file"
                         :host     myhost
                         :path     (append-extension path)
                         :query    query
                         :fragment fragment)))))))
            ((sxpath '(// @ href)) sxml)))

(define (MDC-xhtml->sxml path)
  (and-let* ((source (file->string path))
             (cleaned (regexp-replace-all*
                       source
                       #/\"return false\;\"\"/
                       "\"return false;\""
                       #/onclick=\"[^\"]+\"/
                       ""
                       #/&&/
                       "&amp;&amp;")))
    (call-with-input-string cleaned
      (lambda (in)
        (ssax:xml->sxml in '((xhtml . "http://www.w3.org/1999/xhtml")))))))

(define (process! path)
  (define (format-sxml-to-string sxml)
    (regexp-replace-all*
     (call-with-output-string (cut srl:sxml->html sxml <>))
     #/<xhtml:/ 
     "<"
     #/<\/xhtml:/
     "</"
     #/xmlns:xhtml/ 
     "xmlns"
     #/<useless\/>/
     ""
     ))
  (define (save-to path)
    (let* ((save-path (build-path prefix path)))
      (receive (save-dir _ _) (decompose-path save-path)
        (values save-dir save-path))))
  (define base (rxmatch-if (#/developer\.mozilla\.org(.*)/ path)
                   (#f x)
                   (sys-dirname x)
                   #f))

  (when verbose (print path))
  (let1 sxml (MDC-xhtml->sxml path)
    (remove-useless-elements! sxml)
    (process-links! base sxml)
    (expand-div! sxml)
    (receive (save-dir save-path) (save-to path)
      (create-directory* save-dir)
      (call-with-output-file save-path
        (lambda (out)
          (call-with-input-string (format-sxml-to-string sxml)
            (lambda (in)
              (copy-port in out))))))))

(define (main args)
  (let-args (cdr args)
      ((v      "v|verbose")
       (p      "p|prefix=s" (build-path (current-directory) "out"))
       (help   "h|help" => (cut show-help (car args)))
       . restargs)
    (set! verbose v)
    (set! prefix  p)
    (for-each process! (filter file-is-regular? restargs)))
  0)

(define (show-help prog-name)
  (format #t "usage: gosh main.scm [OPTIONS]... \n")
  (format #t " -v, --verbose     verbose.\n")
  (format #t " -p, --prefix=s    save to.\n")
  (format #t " -h, --help        print this documentation.\n")
  #t)