(use srfi-1)
(use srfi-13)
(use file.util)
(use rfc.uri)
(use gauche.parseopt)
(use sxml.ssax)
(use sxml.sxpath)
(use sxml.tools)
(use sxml.tree-trans)

(load "./common.scm")

(define (getElementById id sxml)
  ((if-car-sxpath `(// (* (@ id (equal? ,id))))) sxml))

(define (escape-text string)
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
  `(texinfo "\n@heading " ,(sxml:string-value node) "\n"))

(define (h4 node)
  `(texinfo "\n@subheading " ,(sxml:string-value node) "\n"))

(define (h5 node)
  `(texinfo "\n@subsubheading " ,(sxml:string-value node) "\n"))

(define (para node)
  `(texinfo ,(sxml:string-value node)))

(define (bold node)
  `(texinfo ,(sxml:string-value node)))
  ;;`(texinfo "@strong{" ,(sxml:string-value node) "}"))

(define (pre node)
  `(texinfo
    "\n@example\n"
    ,(sxml:string-value node)
    "@end example\n"))

;;TODO @image
(define (img node)
  (or (and-let* ((src (sxml:attr node 'src)))
        `(texinfo "@uref{" ,src "," (sys-basename src) "}"))
      ()))

(define (dl node)
  `(texinfo
    "\n@table @code\n"
    ,@(append-map (lambda (elt)
                    (case (sxml:name elt)
                      ((xhtml:dt) `("@item " ,(sxml:string-value elt) "\n"))
                      ((xhtml:dd) `(,(sxml:string-value elt) "\n"))
                      (else
                       ;;(format (current-error-port) "warning:~a\n" (sxml:string-value elt))
                       `("warning:" ,(sxml:string-value elt)))))
                  (sxml:child-elements node))
    "\n@end table\n"))

(define (ul node)
  `(texinfo
    "\n@itemize @bullet\n"
    ,@(append-map (lambda (elt)
                    (case (sxml:name elt)
                      ((xhtml:li) `("@item " ,(sxml:string-value elt) "\n"))
                      (else       `("warning:" ,(sxml:string-value elt)))))
                  (sxml:child-elements node))
    "\n@end itemize\n"))

(define (ol node)
  `(texinfo
    "\n@enumerate\n"
    ,@(append-map (lambda (elt)
                    (case (sxml:name elt)
                      ((xhtml:li) `("@item " ,(sxml:string-value elt) "\n"))
                      (else       `("warning:" ,(sxml:string-value elt)))))
           (sxml:child-elements node))
    "\n@end enumerate\n"))

(define (table node)
  (define (calc-columnfractions node)
    (define (column-length node)
      (apply max (cons 1 (map (lambda (tr)
                                (length ((sxpath '(// (or@ xhtml:th xhtml:td))) tr)))
                              ((sxpath '(// xhtml:tr)) node)))))

    (let1 col (column-length node)
      (if (= col 1)
        "1"
        (string-join (make-list col (format #f ".~d" (quotient 100 col))) " "))))

  `(texinfo
    "\n@multitable @columnfractions " ,(calc-columnfractions node) "\n"
    ,@(append-map (lambda (tr)
                    (or (and-let* ((tdths ((if-sxpath '(// (or@ xhtml:td xhtml:th))) tr)))
                          (cons "@item "
                                (cdr (append-map (lambda (td)
                                                   `("@tab " ,(sxml:string-value td) "\n"))
                                                 tdths))))
                        ()))
                  ((sxpath '(// xhtml:tr)) node))
    "\n@end multitable\n"))

(define (code node)
  (if-let1 cindex ((if-car-sxpath '(texinfo @ cindex *text*)) node)
           `(texinfo "@code{" ,(sxml:string-value node) "}" "\n@cindex " ,cindex "\n")
           `(texinfo "@code{" ,(sxml:string-value node) "}")))

(define (ahref node)
  (define (trim-fragment path)
    (or (string-scan path "#" 'before) path))

  (let1 href ((if-car-sxpath '(@ href *text*)) node)
    (if href
      (let* ((body ((if-car-sxpath '(*text*)) node))
             (internal? (rxmatch-if (#/^file:\/\// href)
                            (#f)
                            #t
                            #f))
             (cmd  (if internal? "@ref" "@uref"))
             (link (if internal?
                     (file-path->texinfo-node (trim-fragment href))
                     href))
             (aux  (if link "" "(404)"))
             (link (cond
                    ((not link) "Top")
                    ((string=? "" link) "Top")
                    (else link))))
        (cond
         ((and body (proper-for-cindex? body))
          `(texinfo (@ (cindex ,body))
                    ,cmd "{" ,link "," ,body ,aux "," ,body "}."))
         (body
          `(texinfo ,cmd "{" ,link "," ,body ,aux "," ,body "}."))
         (else
          `(texinfo ,cmd "{" ,link "}."))))
      ())))

(define (div node)
  (or (and-let* ((class (sxml:attr node 'class))
                 ((string=? class "note")))
        `(texinfo "@footnote{" ,(sxml:string-value node) "}\n"))
      node))

(define (***file-path->texinfo-node path)
  (define (check str)
    (hash-table-get notfound-table str str))

  (rxmatch-cond
    ((#/developer\.mozilla\.org\/(.*)\.html$/ path)
     (#f node)
     (check (replace-all-period-to-underscore node)))
    ((#/developer\.mozilla\.org\/(.*)/ path)
     (#f node)
     (call-with-output-file "./hoge" (lambda (out)
                                       (display path out)
                                       (newline out))
                            :if-exists :append)
     #f)
    (else
     (call-with-output-file "./huga" (lambda (out)
                                       (display path out)
                                       (newline out))
                            :if-exists :append)
     #f)))

(define file-path->texinfo-node (memoize ***file-path->texinfo-node))

(define (proper-for-cindex? str)
  (boolean (#/^\w+$/ str)))

(define-class <texinfo-node> ()
  (self next prev up (children :init-value ())))

(define (make-texinfo-nodes-table order-scm)
  (define (replace-all-period-to-underscore-of-items sxml)
    (pre-post-order
     sxml
     `((item        . ,(lambda (tag text) (list tag (replace-all-period-to-underscore text))))
       (*text*      . ,(lambda (tag text) text))
       (*default*   . ,(lambda x x)))))

  (let* ((sxml (replace-all-period-to-underscore-of-items order-scm))
         (ht (make-hash-table 'string=?)))
    (let1 l ((sxpath '(// item *text*)) sxml)
      (for-each (lambda (prev x next)
                  (hash-table-update! ht x
                                      (lambda (value)
                                        (let1 node (or value (make <texinfo-node>))
                                          (slot-set! node 'self x)
                                          (slot-set! node 'next next)
                                          (slot-set! node 'prev prev)
                                          node))
                                      #f))
                l (cdr l) (append (cddr l) '(""))))
    (let1 proc-brothers ((sxml:preceding-sibling (lambda (node)
                                                   (eq? 'item (sxml:name node))))
                         sxml)
      (for-each (lambda (x)
                  (let ((up (sxml:string-value (car (proc-brothers x))))
                        (children ((sxpath '(item *text*)) x)))
                    (for-each (lambda (item)
                                (let1 node (hash-table-get ht item)
                                  (slot-set! node 'up up)))
                              children)
                    (unless (string=? up "Top")
                      (let1 node-up (hash-table-get ht up)
                        (slot-set! node-up 'children children)))))
                ((sxpath '(// (or@ chapter section subsection subsubsection subsubsubsection)))
                 sxml)))
    ht))

(define (texinfo-menu path)
  (or (and-let* ((node (file-path->texinfo-node path))
                 (parent (hash-table-get texinfo-nodes-table node))
                 (children (slot-ref parent 'children))
                 ((not (null? children))))
        `("@menu"
          ,@(map (cut string-append "* " <> " ::")
                 children)
          "@end menu"))
      ()))

(define (sxml->texinfo-like-sxml sxml)
  (pre-post-order
   (getElementById "pageText" sxml)
   `((*text*      . ,(lambda (tag text) (escape-text text)))
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
     (*default*   . ,(lambda x x)))))

(define (print-texinfo-like-sxml sxml)
  (pre-post-order sxml
   `((texinfo   . ,(lambda x (print (sxml:string-value x)) ()))
     (*text*    . ,(lambda (tag text) text))
     (*default* . ,(lambda x x)))))

(define (process path)
  (define (print-cindex str)
    (when (proper-for-cindex? str)
      (print "@cindex " (escape-text str))))
  (define (save-to path)
    (rxmatch-if (#/developer\.mozilla\.org\/(.*)\.html$/ path)
        (#f x)
        (build-path prefix "developer.mozilla.org"
                    (string-append (replace-all-period-to-underscore x) ".texi"))
        (error "oops." path)))
  (define (at-node path)
    (let* ((node (file-path->texinfo-node path))
           (obj (hash-table-get texinfo-nodes-table node)))
      (string-join (map (cut slot-ref obj <>)
                        '(self next prev up))
                   ",")))
  (define (inner-func path proc)
    (when (file-is-regular? path)
      (let1 save-path (save-to path)
        (unless (and debug
                     (file-exists? save-path)
                     (not (zero? (file-size save-path))))
          (when verbose
            (display path (current-error-port))
            (newline (current-error-port)))
          (create-directory* (sys-dirname save-path))
          (with-output-to-file save-path proc :encoding 'utf-8)))))

  (if (hash-table-exists? needbrowser-table (file-path->texinfo-node path))
    (inner-func
     path
     (lambda ()
       (let1 title (path-sans-extension (sys-basename path))
         (print "@node " (at-node path))
         (print "@chapter " title)
         (print-cindex title)
         (for-each print (texinfo-menu path))
         (print "This page requires an Web browser. Please visit "
                "@uref{"
                (uri-compose
                 :scheme "file"
                 :path   (build-path (current-directory) path))
                "," title "," title "}."))))
    (inner-func
     path
     (lambda ()
       (let ((sxml (load-xml path)))
         (print "@node "    (at-node path))
         (print "@chapter " (escape-text (sxml:string-value (getElementById "title" sxml))))
         (print-cindex (sxml:string-value (getElementById "title" sxml)))
         (for-each print (texinfo-menu path))
         (print-texinfo-like-sxml (sxml->texinfo-like-sxml sxml)))))))

(define prefix #f)
(define verbose #f)
(define debug #f)
(define order-scm #f)
(define notfound-table    (make-hash-table 'string=?))
(define needbrowser-table (make-hash-table 'string=?))
(define texinfo-nodes-table #f)

(define (main args)
  (let-args (cdr args)
      ((v      "v|verbose")
       (d      "debug")
       (p      "p|prefix=s" (build-path (current-directory) "texi"))
       (order  "o|order=s"    "./order.scm")
       (notf   "n|notfound=s" "./notfound.scm")
       (brow   "b|needbrowser=s" "./needbrowser.scm")
       (help   "h|help" => (cut show-help (car args)))
       . restargs)
    (set! prefix p)
    (set! verbose v)
    (set! debug d)
    (set! order-scm (car (file->sexp-list order)))
    (for-each (lambda (x)
                (hash-table-put! notfound-table (car x) (cdr x)))
              (car (file->sexp-list notf)))
    (for-each (lambda (x)
                (hash-table-put! needbrowser-table x #t))
              (car (file->sexp-list brow)))
    (set! texinfo-nodes-table (make-texinfo-nodes-table order-scm))
    (if debug
      (for-each (lambda (text)
                  (process (string-append "out/developer.mozilla.org/" text ".html")))
                ((sxpath '(// item *text*)) order-scm))
      (for-each process restargs)))
  0)

(define (show-help prog-name)
  (format #t "usage: gosh main.scm [OPTIONS]... \n")
  (format #t " -v, --verbose     verbose.\n")
  (format #t " -p, --prefix=s    save to.\n")
  (format #t " -h, --help        print this documentation.\n")
  #t)
