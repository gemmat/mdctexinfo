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

(define verbose  #f)
(define prefix   "/hoge/out")
(define debug    #f)

(define (remove-useless-elements! sxml)
  (define (f l)
    (for-each (lambda (obj)
                (sxml:change-name! obj 'useless)
                (sxml:change-content!  obj `())
                (sxml:change-attrlist! obj `()))
              l))
  (f ((sxpath '(// (or@ xhtml:script xhtml:iframe))) sxml))
  (f ((sxpath '(// (xhtml:link (@ type  (equal? "application/rss+xml"))))) sxml))
  (f (filter (lambda (obj)
               (or (and-let* ((id (sxml:attr obj 'id))
                              (id (sxml:string-value id)))
                     (member id '("nav-access" "sitetools" "menuPageOptions")))
                   (and-let* ((class (sxml:attr obj 'class))
                              (class (sxml:string-value class)))
                     (equal? class "page-anchors"))))
             ((sxpath '(// xhtml:ul)) sxml)))
  (f (filter (lambda (obj)
               (or (and-let* ((id (sxml:attr obj 'id))
                              (id (sxml:string-value id)))
                     (member id '("deki-page-alerts" "popupMessage" "popupMask" "popupContainer"
                                  "page-tags" "page-files" "MTMessage" "languages" "siteFooter" "printfooter")))
                   (and-let* ((class (sxml:attr obj 'class))
                              (class (sxml:string-value class)))
                     (member class '("siteNav" "siteSearch" "pageBar" "suggestchannels")))
                   (and-let* ((style (sxml:attr obj 'style))
                              (style (sxml:string-value style)))
                     (equal? style "background-color:red; color:white; text-align:center;"))))
             ((sxpath '(// xhtml:div)) sxml))))

(define (remove-elements-confuse-serializer! sxml)
  (for-each (lambda (obj)
              (sxml:change-attrlist!
               obj
               (map (lambda (x)
                      (case (car x)
                        ((id href title)
                         (list (car x) (regexp-replace-all #/</
                                                           (sxml:string-value x)
                                                            "&lt;")))
                        (else x)))
                    (sxml:attr-list obj))))
            ((sxpath '(// (* (@ (or@ id href title))))) sxml))
  (for-each (lambda (obj)
              (sxml:change-attrlist!
               obj
               (remove (lambda (x)
                         (memq (car x) '(nowrap border)))
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
            ((sxpath '(// (xhtml:span (@ id (equal? "id"))))) sxml))
  (for-each (lambda (obj)
              (for-each (lambda (obj)
                          (sxml:change-name! obj 'xhtml:span)
                          (sxml:change-attrlist!
                           obj
                           (remove (lambda (x)
                                     (eq? (car x) 'href))
                                   (sxml:attr-list obj))))
                        ((sxpath '(xhtml:a)) obj)))
            ((sxpath '(// (xhtml:p (@ class (equal? "pageLastchange"))))) sxml)))

(define (expand-div! sxml)
  (for-each (lambda (obj)
              (when (and (string=? "" (sxml:string-value obj))
                         (null? (sxml:child-elements obj)))
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
                        (if (and ext (string=? ext "html"))
                          (path-sans-extension r)
                          r))
                      path)
          :query    query
          :fragment fragment))))

(define resolve-uri (memoize ***resolve-uri))

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
  ;;Test   "http://developer.mozilla.org/#hoge"
  ;;Expect "#hoge"
  (receive (scheme _ host _ path query fragment) (uri-parse uri)
    (if (and scheme host path)
      (if (and (string=? scheme "https")
               (string=? host   "developer.mozilla.org"))
        (if (string=? path "/")
          (and fragment (string-append "#" fragment))
          (uri-compose
           :scheme   "file"
           :host     (build-path prefix "developer.mozilla.org")
           :path     (rxmatch-cond
                       ((#/^\/skins|deki\// path)
                        (#f)
                        path)
                       (else (string-append (path-filter path) ".html")))
           :query    query
           :fragment fragment))
        (uri-compose
         :scheme   scheme
         :host     host
         :path     (path-filter path)
         :query    query
         :fragment fragment))
         #f)))

(define offline-uri (memoize ***offline-uri))

(define (process-links! base sxml)
  (define (f base uri)
    (receive (scheme _ host _ path _ _) (uri-parse uri)
      (and
       path
       (cond
        ((and scheme host
              (string=? scheme "https")
              (string=? host   "developer.mozilla.org"))
         (make-place-from-uri-http uri))
        ((and scheme (string=? scheme "file"))
         (make-place-from-uri-file uri))
        ((and (not scheme) (not host))
         (make-place-from-uri-resolve base uri))
        (else
         #f)))))
  (for-each (lambda (obj)
              (and-let* ((uri (sxml:string-value obj))
                         (place (f base uri)))
                (sxml:change-content! obj `(,(uri-file place)))))
            ((sxpath '(// @ (or@ href src))) sxml)))

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
  (or (and-let* ((place (make-place-from-file-path path))
                 (save-path (file-path place))
                 (base (sys-dirname (path-filter (slot-ref place 'path)))))
        (unless (and debug (file-exists? save-path))
          (when verbose (print path))
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
              :encoding 'utf-8)))
        #t)
      (error "oops. " path)))

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
