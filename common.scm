(use sxml.ssax)

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

(define-syntax memoize
  (syntax-rules ()
    ((_ fn)
     (let1 ht (make-hash-table 'equal?)
       (lambda args
         (if (hash-table-exists? ht args)
           (hash-table-get ht args)
           (rlet1 result (apply fn args)
             (hash-table-put! ht args result))))))))
