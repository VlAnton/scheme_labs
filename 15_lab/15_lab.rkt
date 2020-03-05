#lang racket


; Общие

(define left cadr)
(define right caddr)

(define (leaf? tree)
  (andmap empty? (cdr tree))
  )

(define (!= . elems)
  (not (apply eq? elems))
  )


; task_1

(define (get_current_result node left-node right-node)
  (cond
    [(and (number? left-node) (= (remainder node left-node) 0)) node]
    [(and (number? right-node) (= (remainder node right-node) 0)) node]
    [else #f]
    )
  )
  

(define (func tree)
  (cond
    [(or (empty? tree) (leaf? tree)) #f]
    [else
     (let* ([left-node
             (if (empty? (left tree))
                 #f
                 (car (left tree))
                 )
             ]
            [right-node
             (if (empty? (right tree))
                 #f
                 (car (right tree))
                 )
             ]
            [current_res (get_current_result (car tree) left-node right-node)]
            [left_res (func (left tree))]
            [right_res (func (right tree))]
            )
       (cond
         [(number? current_res) current_res]
         [(number? left_res) left_res]
         [(number? right_res) right_res]
         [else #f]
         )
       )
     ]
    )
  )


; task_2

(define (symmetrical_btree tree)
  (cond
    [(or (empty? tree) (leaf? tree)) tree]
    [else
     (list
      (car tree)
      (symmetrical_btree (right tree))
      (symmetrical_btree (left tree))
      )
     ]
    )
  )

; task_3

(define (count_left_branches tree)
  (cond
    [(or (empty? tree) (leaf? tree)) 0]
    [else
     (let ([summand
            (if (not (empty? (left tree)))
                1
                0
                )
            ]
           )
       (+ summand (count_left_branches (left tree)) (count_left_branches (right tree)))
       )
     ]
    )
  )


; 4_task

(define (min_leaf tree)
  (cond
    [(empty? tree) +inf.0]
    [(leaf? tree) (car tree)]
    [else
     (min (min_leaf (left tree)) (min_leaf (right tree)))
     ]
    )
  )


; 5_task


(define (generate_list lst)
  (define sorted-lst (sort lst < #:key cdr))
  (define nodes (append (map car sorted-lst) (list -inf.0)))
  (define i_s_original (map cdr sorted-lst))
  (define i_s_cut (append (cdr i_s_original) (list -inf.0)))

  (reverse
   (cdr
    (foldl
     (λ(node i i1 res)
       (cond
         [(eq? i i1) (cons (append (car res) (list node)) (cdr res))]
         [else (cons (list node) res)]
         )
       ) (list (list (car nodes))) (cdr nodes) i_s_cut i_s_original
         )
    )
   )
  )


(define (tree->list tree)
  (define (iter-tree tree result i)
    (let* ([ltree (left tree)]
           [rtree (right tree)]
           )
      (if (empty? rtree)
          (if (empty? ltree)
              result
              (iter-tree ltree (append result (list (cons (car ltree) i))) (+ i 1)))
          (if (empty? ltree)
              (iter-tree rtree (append result (list (cons (car rtree) i))) (+ i 1))
              (append
               (iter-tree ltree (append result (list (cons (car ltree) i))) (+ i 1))
               (iter-tree rtree (append result (list (cons (car rtree) i))) (+ i 1))
               )
              )
          )
      )
    )
  (if (empty? tree)
      #f
      (cons
       (list (car tree))
       (generate_list (remove-duplicates (iter-tree tree '() 2)))
       )
      )
  )