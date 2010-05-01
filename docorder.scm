(use srfi-1)
(use file.util)
(use srfi-13)
(use gauche.parseopt)
(use sxml.sxpath)

;; find texi/developer.mozilla.org/ja/ -name "*.texi" | sort | gosh docorder.scm > order.scm
;; gosh docorder.scm -r order.scm

(define (mapping-depth depth)
  (case depth
    ((0) "(chapter")
    ((1) "(section")
    ((2) "(subsection")
    ((3) "(subsubsection")
    ((4) "(subsubsubsection")
    ((5) "(subsubsubsection")
    ((6) "(subsubsubsection")))

(define (traverse l)
  (cond
   ((string? l)
    (print "@include " l))
   ((list? l)
    (print "@lowersections")
    (for-each traverse (sxml:child-nodes l))
    (print "@raisesections"))))

(define (main args)
  (let-args (cdr args)
      ((r      "r|read")
       . restargs)
    (if r
      (for-each traverse (file->sexp-list (car restargs)))
      (let ((prev 0))
        (port-for-each (lambda (line)
                         (let* ((depth (- (string-count line #\/) 2))
                                (delta (- depth prev)))
                           (cond
                            ((zero? delta))
                            ((positive? delta)
                             (for-each (lambda (x)
                                         (print (mapping-depth x)))
                                       (iota delta prev)))
                            ((negative? delta)
                             (for-each print (make-list (abs delta) ")"))
                             (print ")")
                             (print (mapping-depth (- depth 1)))))
                           (write line)
                           (newline)
                           (set! prev depth)))
                       read-line)
        (print ")"))))
  0)
