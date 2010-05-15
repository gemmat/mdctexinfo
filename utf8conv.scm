(use file.util)
(use gauche.charconv)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args)
      ((encoding "e|encoding=s")
       . restargs)
    (cond
     ((not encoding)
      (print "specify --encoding=euc_jp\n      --encoding=sjis")
      1)
     ((not (or (string=? encoding "euc_jp")
               (string=? encoding "sjis")))
      (print "only supports --encoding=euc_jp\n      --encoding=sjis")
      1)
     (else
      (for-each (lambda (path)
                  (call-with-input-file path
                    (lambda (in)
                      (let1 dest (string-append encoding "_" path)
                        (create-directory* (sys-dirname dest))
                        (call-with-output-file dest
                          (lambda (out)
                            (copy-port in out))
                          :encoding encoding)))))
                restargs)
      0))))
