;;-----------------------------------------------------------------------------
;;!! Utils to handle multiple values

(define-library (github.com/alvatar/base values)

  (import gambit)

  (export
   uncons
   uncons-2
   uncons-3
   uncons-4
   uncons-cons
   unlist
   unvector
   list->values
   vector->values
   values->list
   values->vector
   values->length
   values-ref
   pred2?+
   eq?+
   eqv?+
   equal?+
   )

  (begin

    ;; List to values (as syntax)
    (define-syntax list->values
      (syntax-rules ()
        ((_ x)
         (apply values x))))

    ;; Vector to values (as syntax)
    (define-syntax vector->values
      (syntax-rules ()
        ((_ x)
         (apply values (vector->list x)))))

    ;;! Values to list
    (define-syntax values->list
      (syntax-rules ()
        ((_ x)
         (call-with-values (lambda () x) list))))

    ;;! Values to vector
    (define-syntax values->vector
      (syntax-rules ()
        ((_ x)
         (call-with-values (lambda () x) vector))))

    ;;! Number of values produced
    (define-syntax values-length
      (syntax-rules ()
        ((_ producer)
         (call-with-values
             (lambda () producer)
           (lambda v (length v))))))

    ;;! Extract only the nth-value from a function returning multiple values
    (define-syntax values-ref
      (syntax-rules ()
        ((_ n producer)
         (call-with-values
             (lambda () producer)
           (lambda v (list-ref v n))))))

    ;;! All values pairs must satisfy the given 2-predicate
    (define-syntax pred2?+
      (syntax-rules ()
        ((_ ?pred ?a ?b)
         (let ((la (values->list ?a))
               (lb (values->list ?b)))
           (let recur ((la la)
                       (lb lb))
             (cond
              ((null? la) (if (null? lb) #t #f))
              ((null? lb) (if (null? la) #t #f))
              (else
               (and (?pred (car la) (car lb))
                    (recur (cdr la)
                           (cdr lb))))))))))

    ;;! All values pairs must satisfy eq?
    (define-syntax eq?+
      (syntax-rules ()
        ((_ ?a ?b)
         (pred2?+ eq? ?a ?b))))

    ;;! All values pairs must satisfy eqv?
    (define-syntax eqv?+
      (syntax-rules ()
        ((_ ?a ?b)
         (pred2?+ eqv? ?a ?b))))

    ;;! All values pairs must satisfy equal?
    (define-syntax equal?+
      (syntax-rules ()
        ((_ ?a ?b)
         (pred2?+ equal? ?a ?b))))

    (include "values.scm")))
