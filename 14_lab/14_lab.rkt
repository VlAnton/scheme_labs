#lang racket


; 4

(define (leaf? subtree)
  (andmap empty? (cdr subtree))
  )

(define (count_leaves tree)
  (cond
    [(empty? tree) 0]
    [(leaf? tree) 1]
    [else
     (+
      (count_leaves (cadr tree))
      (count_leaves (caddr tree))
      )
     ]
    )
  )

; 3

(define (max_in_tree tree)
  (cond
    [(empty? tree) 0]
    [(leaf? tree) (car tree)]
    [else
     (max
      (car tree)
      (max_in_tree (cadr tree))
      (max_in_tree (caddr tree))
      )
     ]
    )
  )


(define (get_counter node max_node)
  (if (= (car node) max_node)
      1
      0
      )
  )

(define (count_maxes tree)
  (define max_node (max_in_tree tree))
  (define (count_recursion tree)
    (cond
      [(empty? tree) 0]
      [else
       (let ([counter (get_counter tree max_node)])
        (+
         counter
         (count_recursion (cadr tree))
         (count_recursion (caddr tree))
         )
        )
       ]
      )
    )
  (count_recursion tree)
  )


; 5

(define (binary? tree)
  (if (or (empty? tree) (leaf? tree))
      #t
      (and (= (length tree) 3) (binary? (cadr tree)) (binary? (caddr tree)))
      )
  )


; 1

(define (get_triplets N_list)
  (define triplets_list
    (foldl
     (λ(x res)
       (define 0_part (car res))
       (define 1_part (cadr res))
       (define 2_part (caddr res))

       (cond
         [(= (remainder x 3) 0) (list (cons x 0_part) 1_part 2_part)]
         [(= (remainder x 3) 1) (list 0_part (cons x 1_part) 2_part)]
         [(= (remainder x 3) 2) (list 0_part 1_part (cons x 2_part))]
         [else res]
         )
       ) '(() () ()) N_list
         )
    )
  (if (not (ormap empty? triplets_list))
      (apply cartesian-product triplets_list)
      #f
      )
  )


; 2

