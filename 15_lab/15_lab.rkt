#lang racket


; Общие

(define (left tree)
  (cadr tree)
  )

(define (right tree)
  (caddr tree)
  )

(define (leaf? tree)
  (andmap empty? (cdr tree))
  )

; task_1

(define (get_current_result node left-val right-val)
  (cond
    [(and (number? left-val) (= (remainder node left-val) 0)) node]
    [(and (number? right-val) (= (remainder node right-val) 0)) node]
    [else #f]
    )
  )
  

(define (func tree)
  (cond
    [(or (empty? tree) (leaf? tree)) #f]
    [else
     (let* ([left-val
             (if (empty? (left tree))
                 #f
                 (car (left tree))
                 )
             ]
            [right-val
             (if (empty? (right tree))
                 #f
                 (car (right tree))
                 )
             ]
            [current_res (get_current_result (car tree) left-val right-val)]
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
     (let ([counter
            (if (not (empty? (left tree)))
                1
                0
                )
            ]
           )
       (+ counter (count_left_branches (left tree)) (count_left_branches (right tree)))
       )
     ]
    )
  )


(define (min_leaf tree)
  (cond
    [(empty? tree) +inf.0]
    [(leaf? tree) (car tree)]
    [else
     (min (min_leaf (left tree)) (min_leaf (right tree)))
     ]
    )
  )