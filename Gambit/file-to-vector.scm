(define (file-to-vector file-name)
  (with-input-from-file file-name
    (lambda ()
      (list->vector (reverse
       (let loop ((char (read-char))
                  (result '()))
         (if (eof-object? char)
             result
             (loop (read-char) (cons char result)))))))))
