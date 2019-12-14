(define-library (github.com/alvatar/base)

  (export and-let*
          cond+
          case+
          ->
          ->>
          aif
          begin0
          push!
          string-null?
          pv
          ps
          let/cc
          dotimes
          )

  (begin

    ;;! SRFI-2 AND-LET*: an AND with local bindings, a guarded LET* special form
    (define-syntax and-let*
      (syntax-rules ()
        ((_ ())
         #t)
        ((_ () ?body ...)
         (begin ?body ...))
        ((_ ((?expr)))
         ?expr)
        ((_ ((?var ?expr)))
         ?expr)
        ((_ (?expr))
         ?expr)
        ((_ ((?expr) ?clauses ...))
         (let ((var ?expr))
           (if var (and-let* (?clauses ...)) var)))
        ((_ ((?var ?expr) ?clauses ...))
         (let ((?var ?expr))
           (if ?var (and-let* (?clauses ...)) ?var)))
        ((_ ((?var ?expr) ?clauses ...) ?body ...)
         (let ((?var ?expr))
           (if ?var (and-let* (?clauses ...) ?body ...) #f)))
        ((_ ((?expr) ?clauses ...) ?body ...)
         (if ?expr (and-let* (?clauses ...) ?body ...) #f))
        ((_ (?var ?clauses ...) ?body ...)
         (if ?var (and-let* (?clauses ...) ?body ...) #f))))

    ;;! SRFI-61 A more general cond clause
    (define-syntax $$cond/maybe-more
      (syntax-rules ()
        ((_ test consequent)
         (if test
             consequent))
        ((_ test consequent clause ...)
         (if test
             consequent
             (cond clause ...)))))

    (define-syntax cond+
      (syntax-rules (=> else)
        ((_ (else else1 else2 ...))
         ;; the (if #t (begin ...)) wrapper ensures that there may be no
         ;; internal definitions in the body of the clause.  R5RS mandates
         ;; this in text (by referring to each subform of the clauses as
         ;; <expression>) but not in its reference implementation of cond,
         ;; which just expands to (begin ...) with no (if #t ...) wrapper.
         (if #t (begin else1 else2 ...)))
        ((_ (test => receiver) more-clause ...)
         (let ((t test))
           ($$cond/maybe-more t
                              (receiver t)
                              more-clause ...)))
        ((_ (generator guard => receiver) more-clause ...)
         (call-with-values (lambda () generator)
           (lambda t
             ($$cond/maybe-more (apply guard    t)
                                (apply receiver t)
                                more-clause ...))))
        ((_ (test) more-clause ...)
         (let ((t test))
           ($$cond/maybe-more t t more-clause ...)))
        ((_ (test body1 body2 ...) more-clause ...)
         ($$cond/maybe-more test
                            (begin body1 body2 ...)
                            more-clause ...))))

    ;;! SRFI-87 => in case clauses
    ;; Included in Alexpander for native availability
    (define-syntax case+
      (syntax-rules (else =>)
        ((case (key ...)
           clauses ...)
         (let ((atom-key (key ...)))
           (case atom-key clauses ...)))
        ((case key
           (else => result))
         (result key))
        ((case key
           ((atoms ...) => result))
         (if (memv key '(atoms ...))
             (result key)))
        ((case key
           ((atoms ...) => result)
           clause clauses ...)
         (if (memv key '(atoms ...))
             (result key)
             (case key clause clauses ...)))
        ((case key
           (else result1 result2 ...))
         (begin result1 result2 ...))
        ((case key
           ((atoms ...) result1 result2 ...))
         (if (memv key '(atoms ...))
             (begin result1 result2 ...)))
        ((case key
           ((atoms ...) result1 result2 ...)
           clause clauses ...)
         (if (memv key '(atoms ...))
             (begin result1 result2 ...)
             (case key clause clauses ...)))))

    ;;! Threading ->
    (define-syntax ->
      (syntax-rules ()
        ((_ ?value) ?value)
        ((_ ?value ?snd ?rest ...)
         (cond
          ((list? '?snd)
           (let ((f (primitive-eval (car '?snd)))
                 (args (cons ?value (cdr '?snd))))
             (-> (apply f args) ?rest ...)))
          ((procedure? ?snd)
           (-> (?snd ?value) ?rest ...))))))

    ;;! Threading ->>
    (define-syntax ->>
      (syntax-rules ()
        ((_ value) value)
        ((_ value snd rest ...)
         (cond
          ((list? 'snd)
           (let ((f (primitive-eval (car 'snd)))
                 (args (append (cdr 'snd) (list value))))
             (->> (apply f args) rest ...)))
          ((procedure? snd)
           (->> (snd value) rest ...))))))

    ;;! Anaphoric if
    (define-syntax aif
      (syntax-rules ()
        ((_ var expr iftrue)
         (let ((var expr))
           (if var
               iftrue
               #f)))
        ((_ var expr iftrue iffalse)
         (let ((var expr))
           (if var
               iftrue
               iffalse)))
        ((_ var pred expr iftrue iffalse)
         (let ((var expr))
           (if (pred var)
               iftrue
               iffalse)))))
    ;;! begin0
    ;; Execute a sequence of forms and return the result of the _first_ one.
    ;; Typically used to evaluate one or more forms with side effects and
    ;; return a value that must be computed before
    ;; Equivalent low-level macro:
    ;; (##define-macro (begin0 . args)
    ;;   (let ((form1 (car args))
    ;;         (rest-forms (cdr args))
    ;;         (var (gensym)))
    ;;     `(let ((,var ,form1)) ,@rest-forms ,var)))
    (define-syntax begin0
      (syntax-rules ()
        ((_ form form1 ... )
         (let ((val form)) form1 ... val))))

    ;;! push!
    ;; Prepend an ITEM to a LIST, like a Lisp macro PUSH an ITEM can be an
    ;; expression, but ls must be a VAR
    ;; Equivalent low-level macro:
    ;; (##define-macro (push! list obj)
    ;;   `(set! ,list (cons ,obj ,list)))
    (define-syntax push!
      (syntax-rules ()
        ((_ item ls)
         (set! ls (cons item ls)))))

    ;;! Pretty-print for values, returning values too
    ;; Equivalent low-level macro:
    ;; (##define-macro (pv form)
    ;;   `(call-with-values
    ;;        (lambda () ,form)
    ;;      (lambda args
    ;;        (for-each pp args)
    ;;        (apply values args))))
    (define-syntax pv
      (syntax-rules ()
        ((_ ?form)
         (call-with-values
             (lambda () ?form)
           (lambda args
             (for-each pp args)
             (apply values args))))))

    ;;! Pretty-print for values, pause execution after (for debugging)
    ;; Equivalent low-level macro:
    ;; (##define-macro (ps form)
    ;;   `(call-with-values
    ;;        (lambda () ,form)
    ;;      (lambda args
    ;;        (for-each pp args)
    ;;        (step)
    ;;        (apply values args))))
    (define-syntax ps
      (syntax-rules ()
        ((_ ?form)
         (call-with-values
             (lambda () ?form)
           (lambda args
             (for-each pp args)
             (step)
             (apply values args))))))

    ;;! Letcc macro (hoping and skipping)
    ;; (##define-macro (let/cc . args)
    ;;   `(call-with-current-continuation
    ;;     (lambda (,(car args)) ,@(cdr args))))
    (define-syntax let/cc
      (syntax-rules ()
        ((_ c . body)
         (call-with-current-continuation
          (lambda (c) . body)))))

    ;;! Do a fixed number of times
    (define-syntax dotimes
      (syntax-rules ()
        ((_ (var n res) . body)
         (do ((limit n)
              (var 0 (+ var 1)))
             ((>= var limit) res)
           . body))
        ((_ (var n) . body)
         (do ((limit n)
              (var 0 (+ var 1)))
             ((>= var limit))
           . body))))))
