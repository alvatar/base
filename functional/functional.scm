;;! Get the arity of a procedure
(define (procedure-arity proc)
  (if (##closure? proc)
      (let* ((cc (##closure-code proc))
             (p (##subprocedure-parent cc)))
        (cond ((eq? p ##cprc-prc-req0)
               0)
              ((eq? p ##cprc-prc-req1)
               1)
              ((eq? p ##cprc-prc-req2)
               2)
              ((eq? p ##cprc-prc-req3)
               3)
              ((eq? p ##cprc-prc-req)
               (- (##vector-ref (##closure-ref proc 1) 6) 1))
              (else
               (##subprocedure-nb-parameters cc))))
      (##subprocedure-nb-parameters proc)))