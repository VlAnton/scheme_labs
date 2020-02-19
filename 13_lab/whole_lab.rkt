#lang racket


; Вспомогательные функции

(define (get_max_polynom . polynoms)
  (foldl
   (λ(x res)
     (if (> (length x) (length res))
         x
         res
         )
     ) '() polynoms
       )
  )

(define (extend_polynoms . polynoms)
  (define max_polynom (apply get_max_polynom polynoms))
  (map
   (λ(polynom)
     (define polynom_extension_length (- (length max_polynom) (length polynom)))
     (define polynom_extension (make-list polynom_extension_length 0))
     (append polynom_extension polynom)
     ) polynoms
       )
  )

(define (list->number lst)
  (foldl (λ(x res) (+ (* res 10) x)) 0 lst)
  )

(define (number->list n)
  (define (iter x res)
    (if (= x 0)
        res
        (iter (quotient x 10) (cons (remainder x 10) res))
        )
    )
  (iter n '())
  )

(define (reversed_idxs degree)
  (build-list degree (λ(x) (- degree 1 x)))
  )

; 1

(define (differentiate_polyndrom polyndrom)
  (define (rec polyndrom)
    (define current_degree (- (length polyndrom) 1))

    (if (empty? (cdr polyndrom))
        '()
        (cons (* current_degree (car polyndrom)) (rec (cdr polyndrom)))
        )
    )
  (rec polyndrom)
  )


; 2

(define (sum_of_polynoms . polynoms)
  (define extended_polynoms (apply extend_polynoms polynoms))

  (apply map + extended_polynoms)
  )

; 3

(define (multiply p1 p2)
  (define degrees (reversed_idxs (length p1)))

  (apply
   sum_of_polynoms
   (map
    (λ(x degree)
      (define ext (make-list degree 0))
      (append (map (λ(y)(* x y)) p2) ext)
      )
    p1 degrees
    )
   )
  )

(define (multiply_polynoms . polynoms)
  (if (empty? (cdr polynoms))
      (car polynoms)
      (multiply
       (car polynoms)
       (apply multiply_polynoms (cdr polynoms))
       )
      )
  )


; 4

(define (sum_in_kth_number k . numbers)
  (define num_lists (map number->list numbers))
  (define sum (apply sum_of_polynoms num_lists))

  (define result
    (foldr
     (λ(x res)
       (define rem (cadr res))
       (define new_x (+ x rem))
       (define result (car res))

       (cond
         [(>= new_x k) (list (cons (- new_x k) result) 1)]
         [else (list (cons new_x result) 0)]
         )
       ) (list '() 0) sum
         )
    )
  (define rem (cdr result))
  (define is_correct_data
    (foldl
     (λ(num_list res)
      (and (andmap (λ(x) (< x k)) num_list) res))
     #t
     num_lists
     )
    )
  (cond
    [(not is_correct_data) #f]
    [else
     (if (not (empty? rem))
        (list->number (cons (car rem) (car result)))
        (list->number (car result))
        )
     ]
    )
  )

; 5

(define (get_value f x)
  (define degrees (reversed_idxs (length f)))
  (foldl
   (λ(xk degree res)
     (+ res (* xk (expt x degree)))
     ) 0 f degrees
       )
  )

(define (newton_method x0 f)
  (define df (differentiate_polyndrom f))
  (define eps 0.001)

  (define (iter xn)
    (define xn+1 (- xn (/ (get_value f xn) (get_value df xn))))
    (if (< (abs (- xn+1 xn)) eps)
        xn+1
        (iter xn+1)
        )
  )

  (iter x0)
  )
