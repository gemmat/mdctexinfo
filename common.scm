(define (load-xml path)
  (call-with-input-file path
    (cut ssax:xml->sxml <> '((xhtml . "http://www.w3.org/1999/xhtml")))
    :encoding 'utf-8))

(define (format-sxml-to-string sxml)
  (regexp-replace-all*
   (call-with-output-string (cut srl:sxml->html sxml <>))
   #/<useless\/>/
   ""
   #/<xhtml:/
   "<"
   #/<\/xhtml:/
   "</"
   #/xmlns:xhtml/
   "xmlns"
   ))

(define (replace-all-period-to-underscore str)
  (regexp-replace-all #/\./ str "_"))

(define (memoize fn)
  (let1 ht (make-hash-table 'equal?)
    (lambda args
      (if (hash-table-exists? ht args)
        (hash-table-get ht args)
        (rlet1 result (apply fn args)
               (hash-table-put! ht args result))))))

(define (getElementsByTagName sxml name)
  (append-map (lambda (x)
                (if (eq? (car x) name)
                  (list x)
                  (getElementsByTagName (cdr x) name)))
              sxml))

(define (***path-filter path)
  ;;downcase and replace characters which confuse the texinfo system.
  ;;A period can confuse the Texinfo but we are going to work it at xml2texi.scm
  (define (replace str)
    (regexp-replace-all #/@|,|:|\'|\"|%3a/ str "_"))

  (replace (string-downcase path)))

(define path-filter (memoize ***path-filter))

(define-class <place> ()
  ((path     :init-keyword :path     :init-value #f)
   (query    :init-keyword :query    :init-value #f)
   (fragment :init-keyword :fragment :init-value #f)))

(define (***make-place-from-uri-http uri)
  (receive (_ _ _ _ path query fragment) (uri-parse uri)
    (let* ((path (if (string=? path "/")
                   "/en"
                   path))
           (path (rxmatch-if (#/^\/skins|deki\// path)
                     (#f)
                     path
                     (string-append path ".html"))))
      (make <place> :path path :query query :fragment fragment))))

(define make-place-from-uri-http (memoize ***make-place-from-uri-http))

(define (***make-place-from-uri-file uri)
  (receive (_ _ _ _ path query fragment) (uri-parse uri)
    (and path
         (rxmatch-if (#/\/developer\.mozilla\.org(\/.*)/ path)
             (#f path)
             (make <place> :path path :query query :fragment fragment)
             #f))))

(define make-place-from-uri-file (memoize ***make-place-from-uri-file))

(define (***make-place-from-uri-resolve base uri)
  (receive (_ _ _ _ path query fragment) (uri-parse uri)
    (and path
         (let1 path (if (relative-path? path)
                      (simplify-path (build-path base path))
                      path)
           (make <place> :path path :query query :fragment fragment)))))

(define make-place-from-uri-resolve (memoize ***make-place-from-uri-resolve))

(define (***make-place-from-file-path path)
  (rxmatch-if (#/developer\.mozilla\.org(\/.*)/ path)
      (#f path)
      (make <place> :path path)
      #f))

(define make-place-from-file-path (memoize ***make-place-from-file-path))

;;(d (make-place-from-uri-http "https://developer.mozilla.org/ja.html"))
;;(d (make-place-from-uri-http "https://developer.mozilla.org/ja/about.html"))
;;(d (make-place-from-uri-file "file:///home/teruaki/developer.mozilla.org/ja.html"))
;;(d (make-place-from-uri-resolve "/ja/about/" "../../ja.html"))
;;(d (make-place-from-uri-resolve "/ja" "./about"))
;;(uri-file (make-place-from-uri-resolve "/ja" "./about:file"))
;;(uri-http (make-place-from-uri-resolve "/ja" "./about"))
;;(file-path (make-place-from-uri-resolve "/ja" "./about"))
;;(file-path (make-place-from-file-path "developer.mozilla.org/ja.html"))
;;(d (make-place-from-uri-http "https://developer.mozilla.org/"))
;;(d (make-place-from-file-path "developer.mozilla.org/ja.html"))

(define-method uri-http ((p <place>))
  (uri-compose :scheme   "https"
               :host     "developer.mozilla.org"
               :path     (slot-ref p 'path)
               :query    (slot-ref p 'query)
               :fragment (slot-ref p 'fragment)))

(define-method uri-file ((p <place>))
  (uri-compose :scheme   "file"
               :path     (path-filter
                          (string-append (current-directory)
                                         "/out/developer.mozilla.org"
                                         (slot-ref p 'path)))
               :query    (slot-ref p 'query)
               :fragment (slot-ref p 'fragment)))

(define-method file-path ((p <place>))
  (string-append "out/developer.mozilla.org"
                 (path-filter (slot-ref p 'path))))

(define-method texi-node ((p <place>))
  (rxmatch-if (#/^\/(.*)\.html$/ (slot-ref p 'path))
      (#f x)
      (replace-all-period-to-underscore x)
      (begin
        (when debug
          (call-with-output-file "./huga" (lambda (out)
                                            (display (slot-ref p 'path) out)
                                            (newline out))
                                 :if-exists :append))
        #f)))
