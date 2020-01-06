(define (get ls key #!rest optional)
  (let ((val (assq key ls)))
    (if val
        (cadr val)
        (or optional #f))))

(define (get+ ls key #!rest optional)
  (let ((val (assq key ls)))
    (if val
        (cdr val)
        (or optional #f))))
