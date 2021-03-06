(use file.util)
(use gauche.charconv)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args)
      ((encoding "e|encoding=s")
       (inplace  "i|inplace")
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
                      (let1 dest (build-path (sys-dirname path) (string-append encoding "_" (sys-basename path)))
                        (create-directory* (sys-dirname dest))
                        (call-with-output-file dest
                          (lambda (out)
                            (copy-port in out))
                          :encoding encoding)
                        (when inplace
                          (move-file dest path :if-exists :supersede))))))
                restargs)
      0))))

