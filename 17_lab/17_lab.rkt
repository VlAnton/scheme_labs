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

(define (tree-map func tree)
  (cond
    [(empty? tree) tree]
    [(leaf? tree) (list (func (car tree)) '() '())]
    [else
     (list (func (car tree))
           (tree-map func (left tree))
           (tree-map func (right tree)))
     ]
    )
  )

; 2

(define (foldl-tree func acc tree)
  (cond
    [(empty? tree) acc]
    [(leaf? tree) (func acc (car tree))]
    [else
     (foldl func
            acc
            (list (car tree)
                  (foldl-tree func acc (left tree))
                  (foldl-tree func acc (right tree)))
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