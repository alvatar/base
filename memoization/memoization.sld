(define-library (github.com/alvatar/base memoization)

  (import (scheme base))

  (export define-memoized
          define-memoized/key-gen
          memoize
          memoize/key-gen)

  (begin

    ;;! Macro for memoized function definition (with default key generator)
    ;; (define-memoized (++ a b) (display "hi\n") (+ a b))
    ;; > (++ 2 3)
    ;; hi
    ;; 5
    ;; > (++ 2 3)
    ;; 5
    (define-syntax define-memoized
      (syntax-rules (lambda)
        ((_ (name args ...) body ...)
         (define name
           (letrec ((name (lambda (args ...) body ...)))
             (memoize name))))
        ((_ name (lambda (args ...) body ...))
         (define-memoized (name args ...) body ...))))

    ;;! Macro for memoized function definition (with default key generator)
    ;; (define-memoized/key-gen (++ (lambda (x y) (list x y)) a b) (display "hi\n") (+ a b))
    ;; > (++ 2 3)
    ;; hi
    ;; 5
    ;; > (++ 2 3)
    ;; 5
    (define-syntax define-memoized/key-gen
      (syntax-rules (lambda)
        ((_ (name key-gen args ...) body ...)
         (define name
           (letrec ((name (lambda (args ...) body ...)))
             (memoize/key-gen key-gen name))))
        ((_ name key-gen (lambda (args ...) body ...))
         (define-memoized/key-gen (name key-gen args ...) body ...))))

    (include "memoization.scm")))
