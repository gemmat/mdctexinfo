(use srfi-1)
(use srfi-13)
(use util.list) ;;alist->hash-table
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

(define (h2 node)
  `(texinfo "\n@heading " ,(sxml:string-value node) "\n"))

(define (h3 node)
  `(texinfo "\n@subheading " ,(sxml:string-value node) "\n"))

(define (h4 node)
  `(texinfo "\n@subsubheading " ,(sxml:string-value node) "\n"))

(define (h5 node)
  `(texinfo "\n@subsubheading " ,(sxml:string-value node) "\n"))

(define (para node)
  `(texinfo ,(sxml:string-value node)))

(define (bold node)
  `(texinfo "@strong{" ,(sxml:string-value node) "}"))

(define (pre node)
  `(texinfo
    "\n@example\n"
    ,(sxml:string-value node)
    "@end example\n"))

(define (img node)
  (or (and-let* ((src (sxml:attr node 'src))
                 ((not (string-scan src "@@api")))
                 (file (path-sans-extension (sxml:string-value src))))
        `(texinfo "@image{" ,file  "}"))
      ()))

(define (dl node)
  `(texinfo
    "\n@table @code\n"
    ,@(append-map (lambda (elt)
                    (case (sxml:name elt)
                      ((xhtml:dt) `("@item " ,(sxml:string-value elt) "\n"))
                      ((xhtml:dd) `(,(sxml:string-value elt) "\n"))
                      (else
                       (format (current-error-port) "warning:~a\n" (sxml:string-value elt))
                       `("warning:" ,(sxml:string-value elt)))))
                  (sxml:child-elements node))
    "\n@end table\n"))

(define (ul node)
  `(texinfo
    "\n@itemize @bullet\n"
    ,@(map (lambda (elt)
             (case (sxml:name elt)
               ((xhtml:li) (string-append "@item " (sxml:string-value elt) "\n"))
               (else       (string-append "warning:" (sxml:string-value elt)))))
           (sxml:child-elements node))
    "\n@end itemize\n"))

(define (ol node)
  `(texinfo
    "\n@enumerate\n"
    ,@(map (lambda (elt)
             (case (sxml:name elt)
               ((xhtml:li) (string-append "@item " (sxml:string-value elt) "\n"))
               (else       (string-append "warning:" (sxml:string-value elt)))))
           (sxml:child-elements node))
    "\n@end enumerate\n"))

(define (table node)
  (define (column-length node)
    (apply max (map (lambda (tr)
                      (length ((sxpath '(// (or@ xhtml:th xhtml:td))) tr)))
                    ((sxpath '(// xhtml:tr)) node))))

  (let* ((col (column-length node))
         (fractions (if (= col 1)
                      "1"
                      (string-join (make-list col (format #f ".~d" (quotient 100 col))) " "))))
    `(texinfo
      "\n@multitable @columnfractions " ,fractions "\n"
      ,@(append-map (lambda (tr)
                      (cons "@item "
                            (cdr
                             (append-map (lambda (td)
                                           `("@tab " ,(sxml:string-value td) "\n"))
                                         ((sxpath '(// (or@ xhtml:td xhtml:th))) tr)))))
                    ((sxpath '(// xhtml:tr)) node))
      "\n@end multitable\n")))

(define (code node)
  `(texinfo "@code{" ,(sxml:string-value node) "}"))

(define (ahref node)
  (define (trim-fragment path)
    (or (string-scan path "#" 'before) path))

  (let ((href ((if-car-sxpath '(@ href *text*)) node))
        (body ((if-car-sxpath '(*text*)) node))
        (rel  (or ((if-car-sxpath '(@ rel *text*)) node) "external")))
    (if href
      (let* ((bound (rxmatch-cond
                      ((#/internal/ rel)
                       (#f)
                       'internal)
                      ((#/external/ rel)
                       (#f)
                       'external)
                      ((#/custom nofollow/ rel)
                       (#f)
                       'internal)
                      (else
                       'external)))
             (cmd (case bound
                    ((internal) "@ref")
                    ((external) "@uref")))
             (link (case bound
                     ((internal) (or (file-path->texinfo-node (trim-fragment href)) "Top"))
                     ((external) href))))
        (if body
          `(texinfo ,cmd "{" ,link "," ,body "}")
          `(texinfo ,cmd "{" ,link "}")))
      ())))

(define (div node)
  (or (and-let* ((class (sxml:attr node 'class))
                 ((string=? class "note")))
        `(texinfo "@footnote{" ,(sxml:string-value node) "}\n"))
      node))

(define (file-path->texinfo-node path)
  (define (check str)
    (if (hash-table-exists? notfound str)
      (let1 value (hash-table-get notfound str)
        (if (string=? value "")
          "Top"
          value))
      str))
  (rxmatch-cond
    ((#/developer\.mozilla\.org\/(.*)\.html$/ path)
     (#f node)
     (check (string-downcase node)))
    ((#/developer\.mozilla\.org\/(.*)/ path)
     (#f node)
     (check (string-downcase node)))
    (else #f)))

(define (file-path-up path)
  (string-append (sys-dirname path) ".html"))


(define (texinfo-menu path)
  (or (and-let* ((dir (path-sans-extension path))
                 ((file-is-directory? dir)))
        `("@menu"
          ,@(map (lambda (x)
                   (string-append "* " (file-path->texinfo-node x) " ::"))
                 (directory-list
                  dir
                  :children? #t
                  :add-path? #t
                  :filter (lambda (path)
                            (and (file-is-regular? path)
                                 (string=? "html" (path-extension path))))
                  :filter-add-path? #t))
          "@end menu"))
      ()))

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
    (when verbose 
      (display path (current-error-port))
      (newline (current-error-port)))
    (receive (save-dir save-file debug-file) (save-to path)
      (create-directory* save-dir)
      (with-output-to-file save-file
        (lambda ()
          (let ((sxml (load-xml path)))
            (print "@node " (file-path->texinfo-node path) ",,," (or (file-path->texinfo-node (file-path-up path)) "Top"))
            (print "@section " (sxml:string-value (getElementById "title" sxml)))
            (print "@findex " (sxml:string-value (getElementById "title" sxml)))
            (for-each print (texinfo-menu path))
            (let1 debug-sxml
                (pre-post-order
                 (pre-post-order
                  (getElementById "pageText" sxml)
                  `((*text*      . ,(lambda (tag text) (esc text)))
                    (xhtml:a     . ,(lambda x (ahref x)))
                    (xhtml:b     . ,(lambda x (bold  x)))
                    (xhtml:code  . ,(lambda x (code  x)))
                    (xhtml:pre   . ,(lambda x (pre   x)))
                    (xhtml:img   . ,(lambda x (img   x)))
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
(define notfound (alist->hash-table (car (file->sexp-list "./notfound.scm")) 'string=?))

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
