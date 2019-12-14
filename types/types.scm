(declare (not safe))

;;!! Return the type of the parameter
;; .parameter Any Scheme object
(define (type-of object)
  (cond
   ((##structure? object) (##structure-type object))
   ((list? object) 'list)
   ((pair? object) 'pair)
   ((vector? object) 'vector)
   ((symbol? object) 'symbol)
   ((keyword? object) 'keyword)
   ((boolean? object) 'boolean)
   ((char? object) 'char)
   ((fixnum? object) 'fixnum)
   ((flonum? object) 'float)
   ((integer? object) 'integer)
   ((rational? object) 'rational)
   ((real? object) 'real)
   ((complex? object) 'complex)
   ((string? object) 'string)
   ((null? object) 'null)
   ((procedure? object) 'procedure)
   ((port? object) 'port)
   ((eof-object? object) 'eof-object)
   ((eq? object (void)) 'void)
   ((table? object) 'table)
   (else 'unknown)))

;;! Coerce to a type
;; Based on code by Aubrey Jaffer
(define (coerce result-type obj)
  (define (err) (error 'coerce obj '-> result-type))
  (let ((obj-type (type-of obj)))
    (if (eq? obj-type result-type)
        obj
        (case obj-type
          ((char)
           (case result-type
             ((number integer) (char->integer obj))
             ((string) (string obj))
             ((symbol) (string->symbol (string obj)))
             ((list) (list obj))
             ((vector) (vector obj))
             (else (err))))
          ((integer rational real complex)
           (case result-type
             ((char) (integer->char obj))
             ((atom) obj)
             ((integer) obj)
             ((string) (number->string obj))
             ((symbol) (string->symbol (number->string obj)))
             ((list) (string->list (number->string obj)))
             ((vector) (list->vector (string->list (number->string obj))))
             (else (err))))
          ((string)
           (case result-type
             ((char) (if (= 1 (string-length obj)) (string-ref obj 0) (err)))
             ((atom) (or (string->number obj) (string->symbol obj)))
             ((number integer) (or (string->number obj) (err)))
             ((symbol) (string->symbol obj))
             ((list) (string->list obj))
             ((vector) (list->vector (string->list obj)))
             (else (err))))
          ((symbol)
           (case result-type
             ((char) (coerce (symbol->string obj) 'char))
             ((number integer) (coerce (symbol->string obj) 'number))
             ((string) (symbol->string obj))
             ((atom) obj)
             ((list) (string->list (symbol->string obj)))
             ((vector) (list->vector (string->list (symbol->string obj))))
             (else (err))))
          ((list)
           (case result-type
             ((char) (if (and (= 1 (length obj))
                              (char? (car obj)))
                         (car obj)
                         (err)))
             ((number integer)
              (or (string->number (list->string obj)) (err)))
             ((string) (list->string obj))
             ((symbol) (string->symbol (list->string obj)))
             ((vector) (list->vector obj))
             (else (err))))
          ((vector)
           (case result-type
             ((char) (if (and (= 1 (vector-length obj))
                              (char? (vector-ref obj 0)))
                         (vector-ref obj 0)
                         (err)))
             ((number integer)
              (or (string->number (coerce obj string)) (err)))
             ((string) (list->string (vector->list obj)))
             ((symbol) (string->symbol (coerce obj string)))
             ((list) (list->vector obj))
             (else (err))))
          (else (err))))))

;;! Generic conversion to integer number
(define ->integer
  (let ((fixnum-max-as-flonum (##fixnum->flonum ##max-fixnum)))
    (lambda (n)
      (cond
       ((##fixnum? n) n)
       ((##bignum? n) n)           ; Bignums are integer by definition
       ((##flonum? n) (if (##fl< n fixnum-max-as-flonum)
                          (##flonum->fixnum n)
                          (##flonum->exact-int n)))
       ((##ratnum? n) (##inexact->exact (##floor n)))
       ((##complex? n) (error "complex->integer number conversion not supported"))
       (else (error "Generic ->integer conversion only implemented for numbers"))))))