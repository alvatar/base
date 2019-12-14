(define-library (github.com/alvatar/base functional combinator)

  (import gambit)

  (export
   U
   Y
   Y!
   compose
   recursive-compose
   tail-recursive-compose
   left-section
   right-section
   curry
   uncurry
   complement
   reversed
   andf
   orf
   arguments-chainf
   arguments-eachf
   arguments-allf)

  (include "combinator.scm"))
