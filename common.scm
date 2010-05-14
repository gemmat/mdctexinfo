(use text.html-lite)
(use sxml.tree-trans)

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
