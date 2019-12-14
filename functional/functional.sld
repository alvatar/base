(define-library (github.com/alvatar/base functional)

  (import gambit)

  (export
   %%define-associative-aux ; needs to be exported
   define-associative
   cut
   cute
   curried
   define-curried
   procedure-arity
   )

  (begin

    ;;! Defines a function and an associative function (that will take any number of
    ;; arguments and apply it to the result of the two previous ones, iteratively)
    (define-syntax %%define-associative-aux
      (syntax-rules ()
        ((_ name f)
         (define-syntax name
           (syntax-rules ()
             ((_ arg1 arg2)
              (f arg1 arg2))
             ((_ arg1 arg2 . rest)
              (name (f arg1 arg2) . rest)))))))

    (define-syntax define-associative
      (syntax-rules ()
        ((_ name (f arg1 arg2) body)
         (begin
           (define (f arg1 arg2) body)
           (%%define-associative-aux name f)))))

    ;;------------------------------------------------------------------------------
    ;;! SRFI-26 Notation for Specializing Parameters without Currying
    ;; Sebastian.Egner@philips.com, 5-Jun-2002.
    ;; adapted from the posting by Al Petrofsky <al@petrofsky.org>
    ;; placed in the public domain.

    ;; (srfi-26-internal-cut slot-names combination . se)
    ;;   transformer used internally
    ;;     slot-names  : the internal names of the slots
    ;;     combination : procedure being specialized, followed by its arguments
    ;;     se          : slots-or-exprs, the qualifiers of the macro
    (define-syntax srfi-26-internal-cut
      (syntax-rules (<> <...>)
        ;; construct fixed- or variable-arity procedure:
        ;;   (begin proc) throws an error if proc is not an <expression>
        ((srfi-26-internal-cut (slot-name ...) (proc arg ...))
         (lambda (slot-name ...) ((begin proc) arg ...)))
        ((srfi-26-internal-cut (slot-name ...) (proc arg ...) <...>)
         (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))
        ;; process one slot-or-expr
        ((srfi-26-internal-cut (slot-name ...)   (position ...)      <>  . se)
         (srfi-26-internal-cut (slot-name ... x) (position ... x)        . se))
        ((srfi-26-internal-cut (slot-name ...)   (position ...)      nse . se)
         (srfi-26-internal-cut (slot-name ...)   (position ... nse)      . se))))

    ;;! cut
    (define-syntax cut
      (syntax-rules ()
        ((cut . slots-or-exprs)
         (srfi-26-internal-cut () () . slots-or-exprs))))

    ;; (srfi-26-internal-cute slot-names nse-bindings combination . se)
    ;;   transformer used internally
    ;;     slot-names     : the internal names of the slots
    ;;     nse-bindings   : let-style bindings for the non-slot expressions.
    ;;     combination    : procedure being specialized, followed by its arguments
    ;;     se             : slots-or-exprs, the qualifiers of the macro
    (define-syntax srfi-26-internal-cute
      (syntax-rules (<> <...>)
        ;; If there are no slot-or-exprs to process, then:
        ;; construct a fixed-arity procedure,
        ((srfi-26-internal-cute
          (slot-name ...) nse-bindings (proc arg ...))
         (let nse-bindings (lambda (slot-name ...) (proc arg ...))))
        ;; or a variable-arity procedure
        ((srfi-26-internal-cute
          (slot-name ...) nse-bindings (proc arg ...) <...>)
         (let nse-bindings (lambda (slot-name ... . x) (apply proc arg ... x))))
        ;; otherwise, process one slot:
        ((srfi-26-internal-cute
          (slot-name ...)         nse-bindings  (position ...)   <>  . se)
         (srfi-26-internal-cute
          (slot-name ... x)       nse-bindings  (position ... x)     . se))
        ;; or one non-slot expression
        ((srfi-26-internal-cute
          slot-names              nse-bindings  (position ...)   nse . se)
         (srfi-26-internal-cute
          slot-names ((x nse) . nse-bindings) (position ... x)       . se))))

    ;;! cute
    (define-syntax cute
      (syntax-rules ()
        ((cute . slots-or-exprs)
         (srfi-26-internal-cute () () () . slots-or-exprs))))

    ;;! Define an automatically curryable function
    ;;
    ;; (define-curried (foo x y z) (+ x (/ y z))) ;; foo has arity 3
    ;; ((foo 3) 1 2) ;; (foo 3) is a procedure with arity 2
    ;; ((foo 3 1) 2) ;; (foo 3 2) is a procedure with arity 1
    (define-syntax curried
      (syntax-rules ()
        ((_ () body ...)
         (lambda () body ...))
        ((_ (arg) body ...)
         (lambda (arg) body ...))
        ((_ (arg args ...) body ...)
         (lambda (arg . rest)
           (let ((next (curried (args ...) body ...)))
             (if (null? rest)
                 next
                 (apply next rest)))))))

    (define-syntax define-curried
      (syntax-rules ()
        ((_ (name args ...) body ...)
         (define name (curried (args ...) body ...)))))

    (include "functional.scm")))
