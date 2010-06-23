(use srfi-1)
(use srfi-13)
(use file.util)
(use gauche.charconv)
(use gauche.parseopt)

(define (unescape-texinfo-text str)
  (regexp-replace-all* str
                       #/@@/ "@"
                       #/@{/ "{"
                       #/@}/ "}"))

(define (tangle src-path dst-path)
  (call-with-input-file src-path
    (lambda (in)
      (call-with-output-file dst-path
        (lambda (out)
          (let loop ((inner-@lisp? #f)
                     (buffer       '())
                     (line         (read-line in)))
            (print line)
            (unless (eof-object? line)
              (rxmatch-cond
                ((#/^@lisp/ line)
                 (#f)
                 (loop #t buffer (read-line in)))
                ((#/^@end lisp/ line)
                 (#f)
                 (for-each (lambda (x)
                             (write x out)
                             (newline out))
                           (call-with-input-string (unescape-texinfo-text (string-concatenate buffer))
                             port->sexp-list))
                 (loop #f '() (read-line in)))
                (else
                 (if inner-@lisp?
                   (loop #t (cons line buffer) (read-line in))
                   (loop #f buffer (read-line in))))))))))))

(define (main args)
  (let-args (cdr args)
      ((verbose     "v|verbose")
       (debug-level "d|debug-level=i" 0)
       . restargs)
    (for-each (lambda (path)
                (when verbose (print path))
                (tangle path (path-swap-extension path "scm")))
              (filter file-is-regular? restargs)))
  0)

