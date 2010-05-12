(define (load-xml path)
  (call-with-input-file path
    (cut ssax:xml->sxml <> '((xhtml . "http://www.w3.org/1999/xhtml")))))

(define (format-sxml-to-string sxml)
  (regexp-replace-all*
   (call-with-output-string (cut srl:sxml->html sxml <>))
   #/<xhtml:/
   "<"
   #/<\/xhtml:/
   "</"
   #/xmlns:xhtml/
   "xmlns"
   #/<td\snowrap([>|\s])/
   "<td nowrap=\"nowrap\"\\1"
   #/<span id\/>/
   "<span id=\"id\"/>"
   #/ controls>/
   " controls=\"controls\">"
   #/ border>/
   " border=\"border\">"
   #/<useless\/>/
   ""
   #/<your%20language&gt\;/
   "&lt;your%20language&gt;"
   #/ id=\"operator</
   " id=\"operator&lt;"
   #/Iterator<char/
   "Iterator&lt;char"
   #/Iterator<short/
   "Iterator&lt;short"
   #/<gripper/
   "&lt;gripper"
   ))
