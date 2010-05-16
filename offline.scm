(use srfi-1)
(use srfi-13)
(use file.util)
(use rfc.uri)
(use gauche.parseopt)
(use gauche.charconv)
(use sxml.sxpath)
(use sxml.ssax)
(use sxml.tools)
(use sxml.serializer)

(load "./common.scm")
(load "./xhtml_entity.scm")

(with-module sxml.ssax
  (set! ssax:predefined-parsed-entities
        `(,@(with-module user xhtml-entity)
          ,@ssax:predefined-parsed-entities)))

(define verbose  #f)
(define prefix   "/hoge/out")
(define debug    #f)

(define (remove-useless-elements! sxml)
  (for-each (lambda (obj)
              (sxml:change-name! obj 'useless)
              (sxml:change-content!  obj `())
              (sxml:change-attrlist! obj `()))
            (append
             ((sxpath '(// (or@ xhtml:script xhtml:iframe))) sxml)
             ((sxpath '(// (xhtml:link (@ type  (equal? "application/rss+xml"))))) sxml)
             (filter (lambda (obj)
                       (or (and-let* ((id (sxml:attr obj 'id))
                                      (id (sxml:string-value id)))
                             (or (equal? id "nav-access")
                                 (equal? id "sitetools")))
                           (and-let* ((class (sxml:attr obj 'class))
                                      (class (sxml:string-value class)))
                             (equal? class "page-anchors"))))
                     ((sxpath '(// xhtml:ul)) sxml))
             (filter (lambda (obj)
                       (or (and-let* ((id (sxml:attr obj 'id))
                                      (id (sxml:string-value id)))
                             (or (equal? id "deki-page-alerts")
                                 (equal? id "popupMessage")
                                 (equal? id "popupMask")
                                 (equal? id "popupContainer")
                                 (equal? id "page-tags")
                                 (equal? id "page-files")
                                 (equal? id "MTMessage")))
                           (and-let* ((class (sxml:attr obj 'class))
                                      (class (sxml:string-value class)))
                             (or (equal? class "siteNav")
                                 (equal? class "siteSearch")
                                 (equal? class "pageBar")
                                 (equal? class "suggestchannels")))
                           (and-let* ((style (sxml:attr obj 'style))
                                      (style (sxml:string-value style)))
                             (equal? style "background-color:red; color:white; text-align:center;"))))
                     ((sxpath '(// xhtml:div)) sxml)))))

(define (remove-elements-confuse-serializer! sxml)
  (for-each (lambda (obj)
              (sxml:change-attrlist!
               obj
               (map (lambda (x)
                      (case (car x)
                        ((id href title)
                         (list (car x) (regexp-replace-all* (sxml:string-value x)
                                                            #/</
                                                            "&lt;")))
                        (else x)))
                    (sxml:attr-list obj))))
            ((sxpath '(// (* (@ (or@ id href title))))) sxml))
  (for-each (lambda (obj)
              (sxml:change-attrlist!
               obj
               (remove (lambda (x)
                         (case (car x)
                           ((nowrap border) #t)
                           (else #f)))
                       (sxml:attr-list obj))))
            ((sxpath '(// (or@ xhtml:td xhtml:table (@ (or@ border nowrap))))) sxml))
  (for-each (lambda (obj)
              (sxml:change-attrlist!
               obj
               (remove (lambda (x)
                         (eq? (car x) 'controls))
                       (sxml:attr-list obj))))
            ((sxpath '(// xhtml:video)) sxml))
  (for-each (lambda (obj)
              (sxml:change-attrlist!
               obj
               (remove (lambda (x)
                         (eq? (car x) 'id))
                       (sxml:attr-list obj))))
            ((sxpath '(// (xhtml:span (@ id (equal? "id"))))) sxml)))

(define (expand-div! sxml)
  (for-each (lambda (obj)
              (when (and (string=? "" (sxml:string-value obj))
                         (zero? (length (sxml:child-elements obj))))
                (sxml:change-content! obj '(""))))
            ((sxpath '(// xhtml:div)) sxml)))

(define (***resolve-uri base uri)
  ;;Test1   "window.html"
  ;;Expect1 "https://developer.mozilla.org/en/DOM/window"
  ;;Test2   "https://developer.mozilla.org/en/About"
  ;;Expect2 "https://developer.mozilla.org/en/About"
  (receive (scheme _ host _ path query fragment) (uri-parse uri)
    (and path
         (uri-compose
          :scheme   (or scheme "https")
          :host     (or host "developer.mozilla.org")
          :path     (if (relative-path? path)
                      (let* ((r (simplify-path (build-path base path)))
                             (ext (path-extension r)))
                        (if (string=? ext "html")
                          (path-sans-extension r)
                          r))
                      path)
          :query    query
          :fragment fragment))))

(define resolve-uri (memoize ***resolve-uri))

(define (***path-filter path)
  ;;downcase and replace characters which confuse the texinfo system.
  ;;A period can confuse the Texinfo but we are going to work it at xml2texi.scm
  (define (replace str)
    (regexp-replace-all #/@|,|:|\'|\"|%3a/ str "_"))

  (replace (string-downcase path)))

(define path-filter (memoize ***path-filter))

(define (***offline-uri uri)
  ;;Test   "http://developer.mozilla.org/en/DOM/window.returnValue"
  ;;Expect "file:///home/teruaki/mdctexinfo/out/developer.mozilla.org/en/dom/window.returnvalue.html"
  ;;Test   "http://developer.mozilla.org/en/DOM/About"
  ;;Expect "file:///home/teruaki/mdctexinfo/out/developer.mozilla.org/en/dom/about.html"
  ;;Test   "http://developer.mozilla.org/en/HTML"
  ;;Expect "file:///home/teruaki/mdctexinfo/out/developer.mozilla.org/en/html.html"
  ;;Test   "file:///home/teruaki/mdctexinfo/out/developer.mozilla.org/en/DOM/Window.html"
  ;;Expect "file:///home/teruaki/mdctexinfo/out/developer.mozilla.org/en/dom/window.html"
  ;;Test   "http://developer.mozilla.org/"
  ;;Expect "file:///home/teruaki/mdctexinfo/out/developer.mozilla.org/"
  ;;Test   "http://developer.mozilla.org/skins/common/css.php"
  ;;Expect "file:///home/teruaki/mdctexinfo/out/developer.mozilla.org/skins/common/css.php"
  (receive (scheme _ host _ path query fragment) (uri-parse uri)
    (if (and scheme host path)
      (if (and (string=? scheme "https")
               (string=? host   "developer.mozilla.org"))
        (uri-compose
         :scheme   "file"
         :host     (build-path prefix "developer.mozilla.org")
         :path     (rxmatch-cond
                     ((#/^\/skins|deki\// path)
                      (#f)
                      path)
                     (else (string-append (path-filter path) ".html")))
         :query    query
         :fragment fragment)
        (uri-compose
         :scheme   scheme
         :host     host
         :path     (path-filter path)
         :query    query
         :fragment fragment))
         #f)))

(define offline-uri (memoize ***offline-uri))

(define (process-links! base sxml)
  (for-each (lambda (obj)
              (and-let* ((uri (resolve-uri base (sxml:string-value obj)))
                         (uri (offline-uri uri)))
                (sxml:change-content! obj `(,uri))))
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
                       "&amp;&amp;"
                       )))
    (call-with-input-string cleaned
      (lambda (in)
        (ssax:xml->sxml in '((xhtml . "http://www.w3.org/1999/xhtml")))))))

(define (process! path)
  (define (solve path)
    (rxmatch-cond
      ((#/developer\.mozilla\.org\/(.*)\/(.*)$/ path)
       (#f base after)
       (values base
               (build-path prefix "developer.mozilla.org" (path-filter base) (path-filter after))))
      ((#/developer\.mozilla\.org\/(.*)$/ path)
       (#f after)
       ;;just for "developer.mozilla.org/En.html"
       (values "/"
               (build-path prefix "developer.mozilla.org" (path-filter after))))
      (else
       (error "oops." path))))

  (receive (base save-path) (solve path)
    (unless (and debug (file-exists? save-path))
      (when verbose (print save-path))
      (let1 sxml (MDC-xhtml->sxml path)
        (remove-useless-elements! sxml)
        (remove-elements-confuse-serializer! sxml)
        (process-links! base sxml)
        (expand-div! sxml)
        (create-directory* (sys-dirname save-path))
        (call-with-output-file save-path
          (lambda (out)
            (call-with-input-string (format-sxml-to-string sxml)
              (lambda (in)
                (copy-port in out))))
          :encoding 'utf-8)))))

(define (main args)
  (let-args (cdr args)
      ((v      "v|verbose")
       (p      "p|prefix=s" (build-path (current-directory) "out"))
       (d      "d|debug")
       (help   "h|help" => (cut show-help (car args)))
       . restargs)
    (set! verbose v)
    (set! prefix  p)
    (set! debug   d)
    (for-each process! (filter file-is-regular? restargs)))
  0)

(define (show-help prog-name)
  (format #t "usage: gosh main.scm [OPTIONS]... \n")
  (format #t " -v, --verbose     verbose.\n")
  (format #t " -p, --prefix=s    save to.\n")
  (format #t " -h, --help        print this documentation.\n")
  #t)
