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

(define (extend_polynoms_by . polynoms)
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
  (define extended_polynoms (apply extend_polynoms_by polynoms))

  (apply map + extended_polynoms)
  )