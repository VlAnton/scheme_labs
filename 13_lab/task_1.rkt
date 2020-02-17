#lang racket


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