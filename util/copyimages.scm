(use file.util)
(define (path-filter path)
  (define (replace str)
    (regexp-replace-all #/@|,|:|\'|\"|%3a/ str "_"))
  (replace (string-downcase path)))

(for-each (lambda (src)
            (let* ((dest (build-path "out" (path-filter src)))
                   (dest (or (string-scan dest "?" 'before) dest)))
              (create-directory* (sys-dirname dest))
              (copy-file src dest :if-exists #f)))
          (glob "developer.mozilla.org/@api/deki/files/*/*"))



