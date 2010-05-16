(use file.util)
(use sxml.tools)
(use sxml.sxpath)
(use gauche.parseopt)

(load "./common.scm")

(define (print-ultimate-include sxml)
  (define (traverse l)
    (for-each (lambda (x)
                (if (eq? 'item (sxml:name x))
                  (print "@include texi/developer.mozilla.org/"
                         (replace-all-period-to-underscore (sxml:string-value x))
                         ".texi")
                  (begin
                    (print "@lowersections")
                    (traverse (sxml:child-nodes x))
                    (print "@raisesections"))))
              l))
  (print "@raisesections")
  (traverse ((sxpath '(chapter *)) sxml)))

(define (print-target-files sxml)
  (for-each (lambda (text)
              (print "out/developer.mozilla.org/" text ".html"))
            ((sxpath '(// item *text*)) sxml)))

(define (print-ultimate-menu sxml)
  (for-each (lambda (text)
              (print "* " (replace-all-period-to-underscore text) " ::"))
            ((sxpath '(// chapter item *text*)) sxml)))

(define (main args)
  (let-args (cdr args)
      ((t      "t|texi")
       (m      "m|menu")
       . restargs)
    (let* ((path (car restargs))
           (sxml (car (file->sexp-list path))))
      (cond
       (t (print-ultimate-include sxml))
       (m (print-ultimate-menu sxml))
       (else (print-target-files sxml)))))
  0)
