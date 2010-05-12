(use srfi-1)
(use file.util)

;; find texi/developer.mozilla.org/ja/ -name "*.texi" | sort | gosh docorder.scm > order.scm
;; gosh docorder.scm -r order.scm

(define (mapping-depth depth)
  (vector-ref '#("(chapter" "(section" "(subsection" "(subsubsection" "(subsubsubsection"
                 "(subsubsubsection" "(subsubsubsection" "(subsubsubsection"
                 "(subsubsubsection" "(subsubsubsection" "(subsubsubsection") (- depth 1)))

(define (directory-traverse path proc)
  (let rec ((path path) (depth 0))
    (when (and (proc path depth) (file-is-directory? path))
      (for-each (cute rec <> (+ depth 1))
                (directory-list path :add-path? #t :children? #t)))))

(define (path-sans-html-extension path)
  (regexp-replace #/\.html$/ path ""))

(define (main args)
  (define prev 0)
  (print "(*TOP*")
  (directory-traverse (cadr args)
                      (lambda (path depth)
                        (unless (and (file-is-regular? path) (file-is-directory? (path-sans-html-extension path)))
                          (let1 delta (- depth prev)
                            (cond
                             ((zero? delta) #t)
                             ((positive? delta)
                              (print (mapping-depth depth)))
                             ((negative? delta)
                              (print (string-join (make-list (abs delta) ")") ""))))
                            (print "(item \"" (regexp-replace #/out\/developer\.mozilla\.org\// (path-sans-html-extension path) "") "\")")
                            (set! prev depth)))
                        #t))
  (print ")\n)")
  0)
