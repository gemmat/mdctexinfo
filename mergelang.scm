(use file.util)
(use gauche.parseopt)
(use sxml.sxpath)
(use sxml.tools)
(use sxml.tree-trans)
(use sxml.serializer)

(load "./common.scm")

(define (***change-lang-from-en-to-ja str)
  (rxmatch-if (#/(.*\/developer\.mozilla\.org\/)en(\/.*)/ str)
      (#f before after)
      (string-append before "ja" after)
      #f))

(define change-lang-from-en-to-ja (memoize ***change-lang-from-en-to-ja))

(define (ahref node)
  (and-let* ((href (sxml:attr node 'href))
             (href (change-lang-from-en-to-ja (sxml:string-value href))))
    (sxml:change-attr node `(href ,href))))

(define (convert sxml)
  (pre-post-order
   sxml
   `((xhtml:a     . ,(lambda x (or (ahref x) x)))
     (*text*      . ,(lambda (tag text) text))
     (*default*   . ,(lambda x x)))))

(define (main args)
  (let-args (cdr args)
      ((verbose "v|verbose")
       . restargs)
    (for-each (lambda (path)
                (and-let* (((file-is-regular? path))
                           (dest-path (change-lang-from-en-to-ja path))
                           ((not (file-exists? dest-path)))
                           (sxml  (load-xml path)))
                  (when verbose (print path))
                  (create-directory* (sys-dirname dest-path))
                  (call-with-output-file dest-path
                    (lambda (out)
                      (call-with-input-string (format-sxml-to-string (convert sxml))
                        (lambda (in)
                          (copy-port in out)
                          (display "\n<!-- not_yet_translated -->\n" out))))
                    :encoding 'utf-8)))
              restargs))
  0)
