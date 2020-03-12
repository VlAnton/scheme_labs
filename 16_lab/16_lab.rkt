#lang racket


; Вспомогательные функции

(define (!= . items)
  (not (apply eq? items))
  )

(define (unique-together? code_one code_two)
  (or
   (ormap != code_one (take code_two (length code_one)))
   (ormap != code_one (drop code_two (- (length code_two) (length code_one))))
   )
  )

(define (get_sorted_codes alphabet)
  (map
   (λ(code)
     (if (number? (car code))
         code
         (cdr code)
         )
     )
   (sort alphabet < #:key
         (λ(code)
           (if (number? (car code))
               (length code)
               (length (cdr code))
               )
           )
         )
   )
  )

(define (compare_lists comparator . lists)
  (apply comparator (map length lists))
  )

(define (min_binary b1 b2)
  (cond
    [(< (car b1) (car b2)) b1]
    [(< (car b2) (car b1)) b2]
    [else (min_binary (cdr b1) (cdr b2))]
    )
  )

(define (min_code code1 code2)
  (cond
    [(!= (length code1) (length code2))
     (if (compare_lists < code1 code2)
         code1
         code2
         )
     ]
    [else (min_binary code1 code2)]
    )
  )


; 1

(define (unambiguous? alphabet)
  (define sorted-codes (get_sorted_codes alphabet))

  (define (iter codes result)
    (cond
      [(not result) result]
      [(empty? (cddr codes)) (unique-together? (car codes) (cadr codes))]
      [else (iter (cdr codes) (and (unique-together? (car codes) (cadr codes)) result))]
      )
    )
  (iter sorted-codes #t)
  )


; 2

(define (add-extra? alphabet)
  (define (iter code)
    (define new_code-0 (append code '(0)))
    (define new_code-1 (append code '(1)))

    (cond
      [(unambiguous? (cons new_code-0 alphabet)) new_code-0]
      [(unambiguous? (cons new_code-1 alphabet)) new_code-1]
      [else
       (min_code
        (iter new_code-0)
        (iter new_code-1)
        )
       ]
      )
    )
  (iter '())
  )

; 3

