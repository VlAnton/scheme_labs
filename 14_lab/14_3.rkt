#lang racket


(define (leaf? subtree)
  (andmap empty? (cdr subtree))
  )


(define (left tree)
  (cadr tree)
  )

(define (right tree)
  (caddr tree)
  )

(define (get_max_pair pair1 pair2 current_max current_counter)  ; TODO: do the logic
  (define max1 (car pair1))
  (define max2 (car pair2))
  (cond
    [(= max1 max2)
     (if (= max1 current_max)
         (cons max1 (- (+ (cdr pair1) (cdr pair2)) current_counter))
         (cons max1 (+ (cdr pair1) (cdr pair2)))
         )
     ]
    [(> max1 max2) pair1]
    [else pair2]
    )
  )


(define (get_max_data tree current_max counter)
  (define node (car tree))
  (cond
    [(= current_max node) (cons current_max (+ counter 1))]
    [(< current_max node) (cons node 1)]
    [else (cons current_max counter)]
    )
  )

(define (count_maxes tree)
  (define (rec tree current_max counter)
    (cond
      [(empty? tree) (cons current_max counter)]
      [(leaf? tree) (get_max_data tree current_max counter)]
      [else
       (let* ([node (car tree)]
              [curr_max_data (get_max_data tree current_max counter)]
              [new_current_max (car curr_max_data)]
              [new_counter (cdr curr_max_data)]
              )
         (get_max_pair
          (rec (left tree) new_current_max new_counter)
          (rec (right tree) new_current_max new_counter)
          new_current_max
          new_counter
          )
         )
       ]
      )
   )
  (cdr (rec tree -inf.0 0))
  )