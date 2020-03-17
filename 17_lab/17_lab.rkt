#lang racket


(define (leaf? tree)
  (andmap empty? (cdr tree))
  )

(define (monotree? tree)
  (ormap empty? (cdr tree))
  )

(define left cadr)
(define right caddr)

; 1

(define (map-tree func tree)
  (cond
    [(empty? tree) tree]
    [(leaf? tree) (list (func (car tree)) '() '())]
    [else
     (list (func (car tree))
           (map-tree func (left tree))
           (map-tree func (right tree)))
     ]
    )
  )

; 2


(define (foldl-tree func result tree)
  (cond
    [(empty? tree) result]
    [(leaf? tree) (func (car tree) result)]
    [else
     (let* ([current_result (func (car tree) result)]
            [left_result (foldl-tree func current_result (left tree))]
            [right_result (foldl-tree func left_result (right tree))])
       right_result
       )
     ]
    )
  )

; 3

(define (get_next_tree tree)
  (if (monotree? tree)
      (if (empty? (left tree))
          (remove-monotrees (right tree))
          (remove-monotrees (left tree))
          )
      (list (car tree)
            (remove-monotrees (left tree))
            (remove-monotrees (right tree))
            )
      )
  )

(define (remove-monotrees tree)
  (cond
    [(empty? tree) tree]
    [(leaf? tree) tree]
    [else (get_next_tree tree)]
    )
  )

; 4

(define (get_sorted_children_roots root edges)
  (sort (filter (λ(edge) (= (car edge) root)) edges)
        <
        #:key cdr
        )
  )

(define left-child cdar)
(define right-child cdadr)

(define (build-tree edges)
  (define sorted-edges (sort edges < #:key car))
  (define (rec root edges)
    (define children (get_sorted_children_roots root edges))
    (cond
      [(empty? children) (list root '() '())]
      [else
       (let* ([lroot (left-child children)]
              [ltree (rec lroot (dropf edges (λ(edge) (= (cdr edge) lroot))))]
              [rroot
               (if (= (length children) 2)
                   (right-child children)
                   '()
                   )
               ]
              [rtree
               (if (number? rroot)
                   (rec rroot (dropf edges (λ(edge) (= (cdr edge) rroot))))
                   rroot
                   )
               ]
              )
         (list root ltree rtree)
         )
       ]
      )
    )
  (if (empty? edges)
      edges
      (rec (caar sorted-edges) sorted-edges)
      )
  )
