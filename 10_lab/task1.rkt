#lang racket


(define (get-proper-number-value char)
  (- (char->integer char) 48)
  )

(define (non-number? number)
  (or (> number 9) (< number 0))
  )

(define (task1 str)
  (if (non-empty-string? str)
      (foldl (λ(char result)
               (define number_value (get-proper-number-value char))

               (if (or (not (exact-nonnegative-integer? result)) (non-number? number_value))
                   #f
                   (+ (* 10 result) number_value)
                   )
               )
             0
             (string->list str)
             )
      #f
      )
  )