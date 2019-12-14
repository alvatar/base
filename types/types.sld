;;------------------------------------------------------------------------------
;;!! Basic types conversion

(define-library (github.com/alvatar/base types)

  (import gambit)

  (export
   symbol->keyword
   keyword->symbol
   ->string
   ->symbol
   ->keyword
   string-append-anything
   symbol-append
   ->integer
   type-of
   coerce
   )

  (begin

    ;;! symbol->keyword
    (define-syntax symbol->keyword
      (syntax-rules ()
        ((_ s)
         (string->keyword (symbol->string s)))))

    ;;! keyword->symbol
    (define-syntax keyword->symbol
      (syntax-rules ()
        ((_ k)
         (string->symbol (keyword->string k)))))

    ;;! Anything to string
    (define-syntax ->string
      (syntax-rules ()
        ((_ o)
         (cond ((string? o) o)
               ((symbol? o) (symbol->string o))
               ((keyword? o) (keyword->string o))
               (else (object->string o))))))

    ;;! Anything to symbol
    (define-syntax ->symbol
      (syntax-rules ()
        ((_ o)
         (string->symbol (->string o)))))

    ;;! Anything to keyword
    (define-syntax ->keyword
      (syntax-rules ()
        ((_ o)
         (string->keyword (->string o)))))

    ;;! Build a string from list of elements (anything)
    (define-syntax string-append-anything
      (syntax-rules ()
        ((_ . ol)
         (apply string-append (map (lambda (e) (->string e)) (list . ol))))))

    ;;! Append anything into a symbol
    (define-syntax symbol-append
      (syntax-rules ()
        ((_ . ol)
         (string->symbol (string-append-anything . ol)))))

    (include "types.scm")))
