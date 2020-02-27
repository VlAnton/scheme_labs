#lang racket


; Вспомогательные функции

(define (leaf? subtree)
  (andmap empty? (cdr subtree))
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
  (if (ormap empty? triplets_list)
      #f
      (apply cartesian-product triplets_list)
      )
  )


; 2

(define (last-index lst element)
  (define idxs (range (length lst)))
  (define (iter lst idxs res)
    (cond
      [(empty? lst) res]
      [(eq? (car lst) element) (iter (cdr lst) (cdr idxs) (cons (car idxs) res))]
      [else (iter (cdr lst) (cdr idxs) res)]
      )
    )
  (define result (iter lst idxs '()))
  (if (empty? result)
      #f
      (car result)
      )
  )


(define (get_full_path directories)
  (define (get_path directories result)
    (define list-head
      (if (empty? directories)
          #f
          (car directories)
          )
      )
    (define idx (last-index result list-head))
    
    (cond
      [(empty? directories) result]
      [(and (number? idx) (= (- (length result) idx) 2))
       (get_path (cdr directories) (append (take result idx) (list list-head)))
       ]
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
    [(empty? tree) -inf.0]
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


(define (get_summand node max_node)
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
       (let ([counter (get_summand tree max_node)])
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
  (cond
    [(empty? tree) #t]
    [(leaf? tree) (not (= (length tree) 3))]
    [else (and (= (length tree) 3) (binary? (cadr tree)) (binary? (caddr tree)))]
    )
  )
