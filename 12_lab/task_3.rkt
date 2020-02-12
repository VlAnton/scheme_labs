#lang racket

(define (find_max_degree a b)
  (define min_number (min a b))
  (define matching_sector (build-list (- min_number 1) (λ(x) (+ x 2))))
  (define possible_nums
    (foldr (λ(n res)
             (if (or (<= (* n n) min_number) (= n min_number))
                 (cons n res)
                 res
                 )
             ) '() matching_sector
               )
    )
  (foldl (λ(x res)
           (define is_degree_a (natural? (inexact->exact (log a x))))
           (define is_degree_b (natural? (inexact->exact (log b x))))

           (cond
             [(not (boolean? res))
              (if (and is_degree_a is_degree_b)
                  x
                  res
                  )
              ]
             [(and is_degree_a is_degree_b) x]
             [else res]
             )
           ) #f possible_nums
             )
  )
  