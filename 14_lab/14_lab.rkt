#lang racket



; 4

(define (leaf? subtree)
  (and (empty? (cadr subtree)) (empty? (caddr subtree)))
  )

(define (count_leaves tree)
  (cond
    [(empty? tree) 0]
    [else
     (if (leaf? tree)
         1
         (+
          (count_leaves (cadr tree))
          (count_leaves (caddr tree))
          )
         )
     ]
    )
  )