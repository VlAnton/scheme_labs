#lang racket


(define (sum_of_polynoms pol_1 pol_2)
  (define max_min_pair
     (if (= (max (length pol_1) (length pol_2)) (length pol_1))
         (cons pol_1 pol_2)
         (cons pol_2 pol_1)
         )
     )
  (define max_pol (car max_min_pair))
  (define min_pol (cdr max_min_pair))

  (define min_pol_extension_length (- (length max_pol) (length min_pol)))
  (define min_pol_extension (make-list min_pol_extension_length 0))
  (define min_pol (append min_pol_extension min_pol))

  (map + max_pol min_pol)
  )