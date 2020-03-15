#lang racket

(define (get_heads_and_tails lists)
  (define (iter heads tails lists)
    (cond
      [(empty? lists) (list heads tails)]
      [else
       (iter (append heads (list (caar lists)))
             (append tails (list (cdar lists)))
             (cdr lists)
             )
       ]
      )
    )
  (iter '() '() lists)
  )


(define (my-map func . lists)
  (cond
    [(empty? (car lists)) '()]
    [else (let* ([h_t_pair (get_heads_and_tails lists)]
                 [heads (car h_t_pair)]
                 [tails (cadr h_t_pair)]
                 )
            (cons (apply func heads)
                  (apply my-map func tails)
                  )
            )
          ]
    )
  )

(define (my-foldl func acc . lists)
  (cond
    [(empty? (car lists)) acc]
    [else (let* ([h_t_pair (get_heads_and_tails lists)]
                 [heads (car h_t_pair)]
                 [tails (cadr h_t_pair)]
                 )
            (apply my-foldl func (apply func acc heads) tails)
            )
          ]
    )
  )