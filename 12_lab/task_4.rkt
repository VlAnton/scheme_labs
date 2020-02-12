#lang racket


(define (get-proper-number-value char)
  (- (char->integer char) 48)
  )

(define (non-number? number)
  (or (> number 9) (< number 0))
  )

(define (sum_of_numbers str)
  (define res_pair (foldl (λ(char res)
                            (define num_val (get-proper-number-value char))
                            (define sum (car res))
                            (define curr_num (cdr res))

                            (if (non-number? num_val)
                                (if (not (empty? (string->list curr_num)))
                                    (cons (+ sum (string->number curr_num)) "")
                                    (cons sum "")
                                    )
                                (cons sum (string-append curr_num (string char)))
                                )
                            ) (cons 0 "") (string->list str))
    )
  (define sum (car res_pair))
  (define num (cdr res_pair))
  (if (and (empty? (string->list num)) (string->number num))
      (+ sum (string->number num))
      sum
      )
  )
