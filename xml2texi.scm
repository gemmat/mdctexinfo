(use srfi-1)
(use file.util)
(use gauche.parseopt)
(use sxml.ssax)
(use sxml.sxpath)
(use sxml.tools)
(use sxml.tree-trans)

(define (load-xml path)
  (call-with-input-file path
    (cut ssax:xml->sxml <> '((xhtml . "http://www.w3.org/1999/xhtml")))))

(define (getElementById id sxml)
  ((if-car-sxpath `(// (* (@ id (equal? ,id))))) sxml))

(define (esc string)
  (regexp-replace-all* string
                       #/@/
                       "@@"
                       #/\{/
                       "@{"
                       #/\}/
                       "@}"))

(define (pre node)
  `(texinfo
    "@example\n"
    ,(sxml:string-value node)
    "@end example"))

(define (h2 node)
  `(texinfo "@heading " ,(sxml:string-value node)))

(define (h3 node)
  `(texinfo "@subheading " ,(sxml:string-value node)))

(define (h4 node)
  `(texinfo "@subsubheading " ,(sxml:string-value node)))

(define (h5 node)
  `(texinfo "@subsubheading " ,(sxml:string-value node)))

(define (para node)
  `(texinfo ,(sxml:string-value node)))

(define (bold node)
  `(texinfo "@strong{" ,(sxml:string-value node) "}"))

(define (dl node)
  `(texinfo
    "\n@table @code\n"
    ,@(append-map (lambda (elt)
                    (case (sxml:name elt)
                      ((xhtml:dt) `("@item " ,(sxml:string-value elt) "\n"))
                      ((xhtml:dd) `(,(sxml:string-value elt) "\n"))
                      (else       `("warning:" ,(sxml:string-value elt)))))
                  (sxml:child-elements node))
    "@end table"))

(define (ul node)
  `(texinfo
    "\n@itemize @bullet\n"
    ,@(map (lambda (elt)
             (case (sxml:name elt)
               ((xhtml:li) (string-append "@item " (sxml:string-value elt) "\n"))
               (else       (string-append "warning:" (sxml:string-value elt)))))
           (sxml:child-elements node))
    "@end itemize\n"))

(define (ol node)
  `(texinfo
    "\n@enumerate\n"
    ,@(map (lambda (elt)
             (case (sxml:name elt)
               ((xhtml:li) (string-append "@item " (sxml:string-value elt) "\n"))
               (else       (string-append "warning:" (sxml:string-value elt)))))
           (sxml:child-elements node))
    "@end enumerate\n"))

(define (table node)
  `(texinfo
    "\n@multitable\n"
    ,@(append-map (lambda (tr)
                    (cons "@item "
                          (cdr
                           (append-map (lambda (td)
                                         `("@tab " ,(sxml:string-value td) "\n"))
                                       ((sxpath '(// (or@ xhtml:td xhtml:th))) tr)))))
                  ((sxpath '(// xhtml:tr)) node))
    "@end multitable"))

(define (code node)
  `(texinfo "@code{" ,(sxml:string-value node) "}"))

(define (ahref node)
  (let ((href ((if-car-sxpath '(@ href *text*)) node))
        (body ((if-car-sxpath '(*text*)) node)))
    (cond
     ((and href body)
      `(texinfo "@uref{" ,href "," ,body "} "))
     (href
      `(texinfo "@uref{" ,href "} "))
     (else
      ()))))

(define (div node)
  (or (and-let* ((class (sxml:attr node 'class))
                 ((string=? class "note")))
        `(texinfo "@footnote{" ,(sxml:string-value node) "}\n"))
      node))

(define (process path prefix)
  (define (save-to path)
    (define aaa (rxmatch-if (#/(developer\.mozilla\.org\/.*)/ path)
                    (#f x)
                    x
                    #f))
    (receive (dir _ _) (decompose-path aaa)
      (values (build-path prefix dir)
              (build-path prefix (path-swap-extension aaa "texi"))
              (build-path prefix (path-swap-extension aaa "scm")))))

  (when (file-is-regular? path)
    (when verbose (print path))
    (receive (save-dir save-file debug-file) (save-to path)
      (create-directory* save-dir)
      (with-output-to-file save-file
        (lambda ()
          (let ((sxml (load-xml path)))
            (print "@section " (sxml:string-value (getElementById "title" sxml)))
            (let1 debug-sxml
                (pre-post-order
                 (pre-post-order
                  (getElementById "pageText" sxml)
                  `((*text*      . ,(lambda (tag text) (esc text)))
                    (xhtml:a     . ,(lambda x (ahref x)))
                    (xhtml:b     . ,(lambda x (bold  x)))
                    (xhtml:code  . ,(lambda x (code  x)))
                    (xhtml:pre   . ,(lambda x (pre   x)))
                    (xhtml:h2    . ,(lambda x (h2    x)))
                    (xhtml:h3    . ,(lambda x (h3    x)))
                    (xhtml:h4    . ,(lambda x (h4    x)))
                    (xhtml:h5    . ,(lambda x (h5    x)))
                    (xhtml:p     . ,(lambda x (para  x)))
                    (xhtml:dl    . ,(lambda x (dl    x)))
                    (xhtml:ul    . ,(lambda x (ul    x)))
                    (xhtml:ol    . ,(lambda x (ol    x)))
                    (xhtml:table . ,(lambda x (table x)))
                    (xhtml:div   . ,(lambda x (div   x)))
                    (*default*   . ,(lambda x x))))
                 `((texinfo   . ,(lambda x (print (sxml:string-value x)) ()))
                   (*default* . ,(lambda x x))))
              (when verbose
                (call-with-output-file debug-file
                  (lambda (out)
                    (write debug-sxml out)))))))))))

(define verbose #f)

(define (main args)
  (let-args (cdr args)
      ((v      "v|verbose")
       (p      "p|prefix=s" (build-path (current-directory) "texi"))
       (help   "h|help" => (cut show-help (car args)))
       . restargs)
    (set! verbose v)
    (for-each (cut process <> p) restargs))
  0)

(define (show-help prog-name)
  (format #t "usage: gosh main.scm [OPTIONS]... \n")
  (format #t " -v, --verbose     verbose.\n")
  (format #t " -p, --prefix=s    save to.\n")
  (format #t " -h, --help        print this documentation.\n")
  #t)
