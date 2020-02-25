#lang racket


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
      (apply cartesian-product triplets_list)  ; TODO: сделать свой cartesian-product, если будет время
      #f
      )
  )


; 2


(define (get_full_path directories)

  (define (get_path directories result)
    (define list-head
      (if (empty? directories)
          #f
          (car directories)
          )
      )
    (define idx (index-of result list-head))
    (cond
      [(empty? directories) result]
      [(and (number? idx)
            (if (= (- (length result) idx) 2)
                (get_path (cdr directories) (append (takef result (λ(x) (not (eq? x list-head)))) (list list-head)))
                (get_path (cdr directories) (append result (list list-head)))
                )
        )]
      [else (get_path (cdr directories) (append result (list list-head)))]
      )
    )

  (define (get_starting_point directories)
    (cond
      [(empty? directories) #f]
      [(eq? (car directories) "C:\\") (get_path directories '())]
      [else (get_starting_point (cdr directories))]
      )
    )

  (get_starting_point directories)
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


; 5

(define (binary? tree)
  (if (or (empty? tree) (leaf? tree))
      #t
      (and (= (length tree) 3) (binary? (cadr tree)) (binary? (caddr tree)))
      )
  )
