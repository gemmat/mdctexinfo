(use srfi-1)
(use srfi-13)
(use file.util)
(use rfc.uri)
(use gauche.parseopt)
(use sxml.ssax)
(use sxml.sxpath)
(use sxml.tools)
(use sxml.tree-trans)
(use sxml.serializer)

(define (load-xml path)
  (call-with-input-file path
    (cut ssax:xml->sxml <> '((xhtml . "http://www.w3.org/1999/xhtml")))))

(define (change-path path from to)
  (string-join (map (lambda (x)
                      (if (string=? x from)
                        to
                        x))
                    (string-split path "/"))
               "/"))

(define (ahref-change-lang node from to)
  (define (helper uri)
    (receive (scheme _ host _ path query fragment) (uri-parse uri)
      (and (string=? scheme "file")
           (uri-compose
            :scheme scheme
            :host   host
            :path   (change-path path from to)
            :query  query
            :fragment fragment))))
  (or (and-let* ((href  (sxml:attr node 'href))
                 (href-new (helper (sxml:string-value href))))
        (sxml:change-attr node `(href ,href-new)))
      node))

(define (format-sxml-to-string sxml)
  (regexp-replace-all*
   (call-with-output-string (cut srl:sxml->html sxml <>))
   #/<xhtml:/ 
   "<"
   #/<\/xhtml:/
   "</"
   #/xmlns:xhtml/ 
   "xmlns"
   ))

(define (main args)
  (define (convert sxml)
    (pre-post-order
     sxml
     `((xhtml:a     . ,(lambda x (ahref-change-lang x "en" "ja")))
       (*text*      . ,(lambda (tag text) text))
       (*default*   . ,(lambda x x)))))
  (define from "en")
  (define to   "ja")
  (let-args (cdr args)
      ((v      "v|verbose")
       . restargs)
    (for-each (lambda (path)
                (and-let* (((file-is-regular? path))
                           (dest (change-path path from to))
                           ((not (file-exists? dest)))
                           (sxml-in  (load-xml path))
                           (sxml-out (convert sxml-in)))
                  (when v (print path))
                  (create-directory* (sys-dirname dest))
                  (call-with-output-file dest
                    (lambda (out)
                      (call-with-input-string (format-sxml-to-string sxml-out)
                        (lambda (in)
                          (copy-port in out)
                          (display "\n<!-- not_yet_translated -->\n" out)))))))
              restargs))
  0)
