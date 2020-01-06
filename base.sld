(define-library (github.com/alvatar/base)

  (export and-let*
          ->
          ->>
          aif
          if-let
          when-let
          begin0
          push!
          string-null?
          pv
          ps
          let/cc
          dotimes
          update!
          unlist
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

    ;;! Threading ->
    (define-syntax ->
      (syntax-rules ()
        [(_ x (y z ...) rest ...)
         (-> (y x z ...) rest ...)]
        [(_ x (f) y ...)
         (-> (f x) y ...)]
        [(_ x f y ...)
         (-> (f x) y ...)]
        [(_ x) x]))

    ;;! Threading ->>
    (define-syntax ->>
      (syntax-rules ()
        [(_ x (y ...) rest ...)
         (->> (y ... x) rest ...)]
        [(_ x (f) y ...)
         (->> (f x) y ...)]
        [(_ x f y ...)
         (->> (f x) y ...)]
        [(_ x) x]))

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

    ;;! if-let
    (define-syntax if-let
      (syntax-rules ()
        ((if-let (var value) consequent ...)
         (let ((var value))
           (if var consequent ...)))))

    ;;! when-let
    (define-syntax when-let
      (syntax-rules ()
        ((when-let (var value) body ...)
         (let ((var value))
           (if var
               (begin body ...))))))

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
           . body))))

    ;;! Update a variable with a function
    (define-syntax update!
      (syntax-rules ()
        ((update! x f)
         (set! x (f x)))))

    ;;! Extract elements from list
    ;; (unlist (a b c) (list 1 2 3) a)
    (define-syntax unlist
      (syntax-rules ()
        ((_ (?val ...) ?form . ?body)
         (apply (lambda (?val ...) . ?body) ?form))))))
