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

(define (max_tree tree1 tree2 tree3)
  (define max_node (max (car tree1) (car tree2) (car tree3)))
  (cond
    [(= max_node (car tree1)) tree1]
    [(= max_node (car tree2)) tree2]
    [(= max_node (car tree3)) tree3]
    )
  )

(define (max_in_tree tree)
  (define (get_max_tree tree)
    (cond
      [(empty? tree) '(0)]
      [(leaf? tree) tree]
      [else
       (max_tree
        tree
        (get_max_tree (cadr tree))
        (get_max_tree (caddr tree))
        )
       ]
      )
    )
  (car (get_max_tree tree))
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