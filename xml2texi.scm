(use srfi-1)
(use srfi-13)
(use util.list) ;;alist->hash-table
(use file.util)
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
  `(texinfo "\n@subheading " ,(sxml:string-value node) "\n"))

(define (h4 node)
  `(texinfo "\n@subsubheading " ,(sxml:string-value node) "\n"))

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
  (define (column-length node)
    (apply max (cons 1 (map (lambda (tr)
                              (length ((sxpath '(// (or@ xhtml:th xhtml:td))) tr)))
                            ((sxpath '(// xhtml:tr)) node)))))

  (let* ((col (column-length node))
         (fractions (if (= col 1)
                      "1"
                      (string-join (make-list col (format #f ".~d" (quotient 100 col))) " "))))
    `(texinfo
      "\n@multitable @columnfractions " ,fractions "\n"
      ,@(append-map (lambda (tr)
                      (or (and-let* ((tdths ((if-sxpath '(// (or@ xhtml:td xhtml:th))) tr)))
                            (cons "@item "
                                  (cdr (append-map (lambda (td)
                                                     `("@tab " ,(sxml:string-value td) "\n"))
                                                   tdths))))
                          ()))
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
                      ((#/^file:/ href)
                       (#f)
                       'internal)
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
                     ((internal) (file-path->texinfo-node (trim-fragment href)))
                     ((external) href))))
        (if body
          `(texinfo ,cmd "{" ,(or link "Top") "," ,body ,(if link "" "(404)") "}")
          `(texinfo ,cmd "{" ,(or link "Top") "}")))
      ())))

(define (div node)
  (or (and-let* ((class (sxml:attr node 'class))
                 ((string=? class "note")))
        `(texinfo "@footnote{" ,(sxml:string-value node) "}\n"))
      node))

(define (replace-all-period-to-underscore str)
  (regexp-replace-all #/\./ str "_"))

(define cache-table (make-hash-table 'string=?))

(define (file-path->texinfo-node path)
  (define (check str)
    (hash-table-get notfound str str))
  (define (cache result)
    (hash-table-put! cache-table path result)
    result)

  (if (hash-table-exists? cache-table path)
    (hash-table-get cache-table path)
    (cache
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
        #f)))))

(define-class <texinfo-node> ()
  (self next prev up (children :init-value ())))

(define (make-texinfo-nodes-table order-file-path)
  (let* ((sxml (car (file->sexp-list order-file-path)))
         (preceding-sibling-item ((sxml:preceding-sibling (lambda (node) (eq? 'item (sxml:name node)))) sxml))
         (ht (make-hash-table 'string=?)))
    (let1 l (map replace-all-period-to-underscore ((sxpath '(// item *text*)) sxml))
      (for-each (lambda (prev x next)
                  (hash-table-update! ht x (lambda (value)
                                             (let1 node (or value (make <texinfo-node>))
                                               (slot-set! node 'self x)
                                               (slot-set! node 'next next)
                                               (slot-set! node 'prev prev)
                                               node))
                                      #f))
                l (cdr l) (append (cddr l) '(""))))
    (for-each (lambda (x)
                (let ((up (replace-all-period-to-underscore (sxml:string-value (car (preceding-sibling-item x)))))
                      (children (map replace-all-period-to-underscore ((sxpath '(item *text*)) x))))
                  (for-each (lambda (item)
                              (let1 node (hash-table-get ht item)
                                (slot-set! node 'up up)))
                            children)
                  (unless (string=? up "Top")
                    (let1 node-up (hash-table-get ht up)
                      (slot-set! node-up 'children children)))))
              ((sxpath '(// (or@ chapter section subsection subsubsection subsubsubsection))) sxml))
    ht))

(define (texinfo-menu path)
  (or (and-let* ((node (hash-table-get texinfo-nodes-table (file-path->texinfo-node path)))
                 (children (slot-ref node 'children))
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
     (*default* . ,(lambda x x)))))

(define (process path)
  (define (save-to path)
    (rxmatch-if (#/developer\.mozilla\.org\/(.*)\.html$/ path)
        (#f x)
        (build-path prefix "developer.mozilla.org" (string-append (replace-all-period-to-underscore x) ".texi"))
        (error "oops." path)))

  (define (path->node path)
    (let1 node (hash-table-get texinfo-nodes-table (file-path->texinfo-node path))
      (string-join (map (cut slot-ref node <>)
                        '(self next prev up))
                   ",")))

  (when (file-is-regular? path)
    (let1 save-path (save-to path)
      (unless (and debug (file-exists? save-path))
        (when verbose
          (display path (current-error-port))
          (newline (current-error-port)))
        (create-directory* (sys-dirname save-path))
        (with-output-to-file save-path
          (lambda ()
            (let ((sxml (load-xml path)))
              (print "@node " (path->node path))
              (print "@section " (escape-text (sxml:string-value (getElementById "title" sxml))))
              (print "@findex " (escape-text (sxml:string-value (getElementById "title" sxml))))
              (for-each print (texinfo-menu path))
              (print-texinfo-like-sxml (sxml->texinfo-like-sxml sxml))))
          :encoding 'utf-8)))))

(define prefix #f)
(define verbose #f)
(define debug #f)
(define notfound #f)
(define texinfo-nodes-table #f)

(define (main args)
  (let-args (cdr args)
      ((v      "v|verbose")
       (d      "debug")
       (p      "p|prefix=s" (build-path (current-directory) "texi"))
       (order  "o|order=s" "./order.scm")
       (notf   "n|notfound=s" "./notfound.scm")
       (help   "h|help" => (cut show-help (car args)))
       . restargs)
    (set! prefix p)
    (set! verbose v)
    (set! debug d)
    (set! texinfo-nodes-table (make-texinfo-nodes-table order))
    (set! notfound (alist->hash-table (car (file->sexp-list notf)) 'string=?))
    (if debug
      (let1 sxml (car (file->sexp-list order))
        (for-each (lambda (text)
                    (process (string-append "out/developer.mozilla.org/" text ".html")))
                  ((sxpath '(// item *text*)) sxml)))
      (for-each process restargs)))
  0)

(define (show-help prog-name)
  (format #t "usage: gosh main.scm [OPTIONS]... \n")
  (format #t " -v, --verbose     verbose.\n")
  (format #t " -p, --prefix=s    save to.\n")
  (format #t " -h, --help        print this documentation.\n")
  #t)
