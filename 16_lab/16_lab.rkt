#lang racket

(define (!= . items)
  (not (apply eq? items))
  )

(define (unique-together? code_one code_two)
  (or
   (ormap != code_one (take code_two (length code_one)))
   (ormap != code_one (drop code_two (- (length code_two) (length code_one))))
   )
  )

(define (unambiguous? alphabet)
  (define sorted-codes
    (map
     cdr
     (sort alphabet < #:key
           (λ(code)
             (length (cdr code))
             )
           )
     )
    )

  (define (iter codes result)
    (cond
      [(not result) result]
      [(empty? (cddr codes)) (unique-together? (car codes) (cadr codes))]
      [else (iter (cdr codes)
                  (and (unique-together? (car codes) (cadr codes)) result)
                  )
            ]
      )
    )
  (iter sorted-codes #t)
  )