#lang racket


(define (task2 str delims)
  (define result_data
    (foldl (λ(char result)
             (define str_val (string char))
             (define counter (car result))
             (define possible_word (cdr result))

             (cond
               [(and (string-contains? delims str_val) (not (non-empty-string? possible_word))) (cons counter "")]
               [(and (string-contains? delims str_val) (non-empty-string? possible_word)) (cons (+ counter 1) "")]
               [else (cons counter (string-append str_val possible_word))]
               )
             )
           (cons 0 "")
           (string->list str)
           )
    )
  (define result (car result_data))
  (define possible_word (cdr result_data))

  (if (non-empty-string? possible_word)
      (+ 1 result)
      result
      )
  )
