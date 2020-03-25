#lang racket


; 1

(define (head-or-false lst)
  (if (not (empty? lst))
      (car lst)
      #f
      )
  )

(define (get-partition lst head)
  (define (iter smaller equal larger lst)
    (define current_head (head-or-false lst))
    (cond
      [(empty? lst) (list smaller equal larger)]
      [(< current_head head) (iter (cons current_head smaller) equal larger (cdr lst))]
      [(= head current_head) (iter smaller (cons current_head equal) larger (cdr lst))]
      [(> current_head head) (iter smaller equal (cons current_head larger) (cdr lst))]
      )
    )
  (iter '() '() '() lst)
  )

(define get-smaller car)
(define get-equal cadr)
(define get-larger caddr)

(define (quick-sort lst)
  (cond
    [(empty? lst) lst]
    [else
     (let ([partition (get-partition lst (car lst))])
       (append (quick-sort (get-smaller partition))
               (get-equal partition)
               (quick-sort (get-larger partition)))
       )
     ]
    )
  )

; 2

(define (sum-of-digits number)
  (define (inner_func number)
    (if (< number 10)
        number
        (+ (remainder number 10) (sum-of-digits (quotient number 10)))
        )
    )
  (abs (inner_func number))
  )

(define (sort-by-sum-of-digits lst)
  (cond
    [(empty? lst) lst]
    [else
     (insert! (cons (sum-of-digits (car lst)) (car lst))
              (sort-by-sum-of-digits (cdr lst)))
     ]
    )
  )

(define (insert! element_pair lst)
  (define element_sum (car element_pair))
  (define element_val (cdr element_pair))

  (if (empty? lst)
      (cons element_val lst)
      (let* ([current_element (car lst)]
             [current_element_sum (sum-of-digits current_element)])
        (cond
          [(< element_sum current_element_sum) (cons element_val lst)]
          [(>= element_sum current_element_sum)
           (cons current_element (insert! element_pair (cdr lst)))
           ]
          )
        )
      )
  )


; 3

(define (get-num-val byte)
  (- byte 48)
  )

(define (is_number? byte)
  (define num_val (get-num-val byte))
  (and (>= num_val 0) (< num_val 10))
  )

(define (get-reversed-number digits)
  (foldr (λ(x res) (+ x (* res 10))) 0 digits)
  )


(define (get-numbers-from file)
  (define in (open-input-file file))
  (define out (open-output-file "output.txt"))

  (define (next)
    (define byte (read-byte in))
    (if (eq? byte eof)
        byte
        (if (is_number? byte)
            (get-num-val byte)
            #f
            )
        )
    )

  (define (iter-file output digits-list sum-of-numbers count-numbers)
    (define data (next))
    (cond
      [(eq? data eof)
       (append output
               (list (number->string sum-of-numbers))
               (list (number->string count-numbers)))
       ]
      [(number? data)
       (iter-file output
                  (cons data digits-list)
                  sum-of-numbers
                  count-numbers)
       ]
      [else
       (let ([num_value (get-reversed-number digits-list)])
         (if (empty? digits-list)
             (iter-file output digits-list sum-of-numbers count-numbers)
             (iter-file (append output (list (number->string num_value)))
                        '()
                        (+ num_value sum-of-numbers)
                        (+ 1 count-numbers))
             )
         )
       ]
      )
    )

  (define (write-out lines)
    (for-each (λ(line) (writeln line out)) lines)
    )

  (write-out (iter-file '() '() 0 0))
  (close-input-port in)
  (close-output-port out)
  )