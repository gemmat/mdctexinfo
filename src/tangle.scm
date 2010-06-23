(use srfi-1)
(use srfi-13)
(use file.util)
(use gauche.charconv)
(use gauche.parseopt)

(define (unescape-texinfo-text str)
  (regexp-replace-all* str
                       #/@@/ "@"
                       #/@\{/ "{"
                       #/@\}/ "}"))

(define (tangle src-path dst-path)
  (call-with-input-file src-path
    (lambda (in)
      (call-with-output-file dst-path
        (lambda (out)
          (let loop ((inner-@lisp? #f))
            (let1 line (read-line in)
              (unless (eof-object? line)
                (rxmatch-cond
                  ((#/^@lisp$/ line)
                   (#f)
                   (loop #t))
                  ((#/^@end lisp$/ line)
                   (#f)
                   (newline out)
                   (loop #f))
                  (else
                   (when inner-@lisp?
                     (display line out)
                     (newline out))
                   (loop inner-@lisp?)))))))))))

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

