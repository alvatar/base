;;! uncons (SRFI-71)
(define (uncons pair)
  (values (car pair) (cdr pair)))

;;! uncons-2 (SRFI-71)
(define (uncons-2 list)
  (values (car list) (cadr list) (cddr list)))

;;! uncons-3 (SRFI-71)
(define (uncons-3 list)
  (values (car list) (cadr list) (caddr list) (cdddr list)))

;;! uncons-4 (SRFI-71)
(define (uncons-4 list)
  (values (car list) (cadr list) (caddr list) (cadddr list) (cddddr list)))

;;! uncons-cons (SRFI-71)
(define (uncons-cons alist)
  (values (caar alist) (cdar alist) (cdr alist)))

;;! unlist (SRFI-71)
(define (unlist list)
  (apply values list))

;;! unvector (SRFI-71)
(define (unvector vector)
  (apply values (vector->list vector)))
