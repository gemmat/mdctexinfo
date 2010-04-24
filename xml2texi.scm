(use srfi-1)
(use file.util)
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

(define (h3 node)
  `(texinfo "@subheading " ,(sxml:string-value node)))

(define (h4 node)
  `(texinfo "@subsubheading " ,(sxml:string-value node)))

(define (h5 node)
  `(texinfo "@subsubheading " ,(sxml:string-value node)))

(define (para node)
  `(texinfo ,(sxml:string-value node)))

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

(define (table node)
  (display node (current-error-port))
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

(define (main args)
  (define (paths path)
    (define out "/home/teruaki/texinfo/jsref/output/")
    (receive (d _ _) (decompose-path path)
      (values (build-path out d)
              (build-path out (path-swap-extension path "texi")))))
  (for-each
   (lambda (path)
     (when (file-is-regular? path)
       (print path)
       (receive (savedir savefile) (paths path)
         (let ((sxml (load-xml path)))
           (create-directory* savedir)
           (with-output-to-file savefile
             (lambda ()
               (print "@section " (sxml:string-value (getElementById "title" sxml)))
               (pre-post-order
                (pre-post-order
                 (getElementById "pageText" sxml)
                 `((*text*      . ,(lambda (tag text) (esc text)))
                   ((@ id (equal? "title")) . ,(lambda x `(texinfo (sxml:string-value x))))
                   (xhtml:a     . ,(lambda x (ahref x)))
                   (xhtml:code  . ,(lambda x (code  x)))
                   (xhtml:pre   . ,(lambda x (pre   x)))
                   (xhtml:h3    . ,(lambda x (h3    x)))
                   (xhtml:h4    . ,(lambda x (h4    x)))
                   (xhtml:h5    . ,(lambda x (h5    x)))
                   (xhtml:p     . ,(lambda x (para  x)))
                   (xhtml:dl    . ,(lambda x (dl    x)))
                   (xhtml:ul    . ,(lambda x (ul    x)))
                   (xhtml:table . ,(lambda x (table x)))
                   (*default*   . ,(lambda x x))))
                `((texinfo   . ,(lambda x (print (sxml:string-value x)) ()))
                  (*default* . ,(lambda x x))))))))))
   (cdr args))
  0)
