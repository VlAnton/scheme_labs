#lang racket


(define (get_max_polynom . polynoms)
  (foldl
   (λ(x res)
     (if (> (length x) (length res))
         x
         res
         )
     ) '() polynoms
       )
  )

(define (extend_polynoms . polynoms)
  (define max_polynom (apply get_max_polynom polynoms))
  (map
   (λ(polynom)
     (define polynom_extension_length (- (length max_polynom) (length polynom)))
     (define polynom_extension (make-list polynom_extension_length 0))
     (append polynom_extension polynom)
     ) polynoms
       )
  )

(define (sum_of_polynoms . polynoms)
  (define extended_polynoms (apply extend_polynoms polynoms))

  (apply map + extended_polynoms)
  )

(define (reversed_idxs degree)
  (build-list degree (λ(x) (- degree 1 x)))
  )

(define (multiply p1 p2)
  (define degrees (reversed_idxs (length p1)))

  (apply
   sum_of_polynoms
   (map
    (λ(x degree)
      (define ext (make-list degree 0))
      (append (map (λ(y)(* x y)) p2) ext)
      )
    p1 degrees
    )
   )
  )

(define (multiply_polynoms . polynoms)
  (if (empty? (cdr polynoms))
      (car polynoms)
      (multiply
       (car polynoms)
       (apply multiply_polynoms (cdr polynoms))
       )
      )
  )

