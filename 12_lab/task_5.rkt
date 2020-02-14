#lang racket


(define (prime_number? n)
  (define (iter i)
    (if (> (* i i) n)
        #t
        (if (= (remainder n i) 0)
            #f
            (iter (+ i 2))
            )
        )
    )
  (cond
    [(= n 1) #f]
    [(= n 2) #t]
    [(odd? (+ n 1)) #f]
    [else (iter 3)]
    )
  )

(define (list->number lst)
  (foldl (λ(x res) (+ (* res 10) x)) 0 lst)
  )

(define (number->list n)
  (define (iter x res)
    (if (= x 0)
        res
        (iter (quotient x 10) (cons (remainder x 10) res))
        )
    )
  (iter n '())
  )

(define (func n)
  (define digits (build-list 10 values))
  (define lst_num (number->list n))
  (define idxs (build-list (+ (length lst_num) 1) values))

  (foldl (λ(x res)
           (define inner_res
             (foldl (λ(idx res_inner)
                    (define new_num_lst (flatten (list (take lst_num idx) x (drop lst_num idx))))
                    (define new_num (list->number new_num_lst))
                    (cond
                      [(not (boolean? res_inner))
                       (if (and (prime_number? new_num) (> new_num res_inner))
                           new_num
                           res_inner
                           )
                       ]
                      [(prime_number? new_num) new_num]
                      [else #f]
                      )
                    ) #f idxs
                      )
             )
           (cond
             [(boolean? inner_res) res]
             [(not (boolean? res))
              (if (and (prime_number? inner_res) (> inner_res res))
                  inner_res
                  res
                  )
              ]
              [(prime_number? inner_res) inner_res]
              [else #f]
             )
           ) #f digits)
  )
