;;------------------------------------------------------------------------------
;;!! Memoization

;;! Function computation memoization specifying a key generation procedure
;; This procedure will be applied to the parameters to construct the key
(define (memoize/key-gen key-gen f)
  (let ((cache (make-table)))
    (lambda args
      (let ((key (apply key-gen args)))
        (apply
         values
         (let ((v (table-ref cache key #f)))
           (or v
               (call-with-values (lambda () (apply f args))
                 (lambda results
                   (table-set! cache key results)
                   results)))))))))

;;! Function computation memoization with default key generation
(define (memoize f)
  (let ((cache (make-table)))
    (lambda k
      (let ((v (table-ref cache k #f)))
	(or v
            (let ((res (apply f k)))
	      (table-set! cache k res)
	      res))))))
