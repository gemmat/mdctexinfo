(use file.util)
(use gauche.charconv)

(define (main args)
  (for-each (lambda (path)
              (call-with-input-file path
                (lambda (in)
                  (let1 dest (string-append "eucjp_" path)
                    (create-directory* (sys-dirname dest))
                    (call-with-output-file dest
                      (lambda (out)
                        (copy-port in out))
                      :encoding 'eucjp)))))
            (cdr args)))
