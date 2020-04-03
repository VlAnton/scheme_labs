#lang racket

; Вспомогательные функции

(define (empty-word? word)
  (= (string-length word) 0)
  )

(define (letter? char)
  (define cyrillic_bytes_segment (cons 1040 1103))
  (define latinic_bytes_segment (cons 65 122))
  (define byte_val (char->integer char))

  (define cyrillic?
    (and (>= byte_val (car cyrillic_bytes_segment))
         (<= byte_val (cdr cyrillic_bytes_segment)))
    )
  (define latin?
    (and (>= byte_val (car latinic_bytes_segment))
         (<= byte_val (cdr latinic_bytes_segment)))
    )

  (or cyrillic? latin?)
  )

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
        (+ (remainder number 10)
           (inner_func (quotient number 10)))
        )
    )
  (inner_func (abs number))
  )

(define (sort-by-sum-of-digits lst)
  (if (empty? lst)
      lst
      (insert! (cons (sum-of-digits (car lst)) (car lst))
               (sort-by-sum-of-digits (cdr lst)))
    )
  )

(define (insert! element_pair lst)
  (define element_sum (car element_pair))
  (define element_val (cdr element_pair))

  (if (empty? lst)
      (list element_val)
      (let* ([current_element (car lst)]
             [current_element_sum (sum-of-digits current_element)])
        (cond
          [(>= element_sum current_element_sum) (cons element_val lst)]
          [(< element_sum current_element_sum) (cons current_element
                                                     (insert! element_pair (cdr lst)))]
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
  (define out (open-output-file #:exists 'replace "output.txt"))

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
    (for-each (λ(line) (fprintf out "~a\n" line)) lines)
    )

  (write-out (iter-file '() '() 0 0))
  (close-input-port in)
  (close-output-port out)
  )

; 4


(define (skip-spaces in out char)
  (define tab? (equal? char #\tab))
  (define space? (equal? char #\space))
  (define newline? (equal? char #\newline))

  (if (or tab? space? newline?)
      (skip-spaces in out (read-char in))
      (fprintf out "~a" (string char))
      )
  )

(define (squash-words file)
  (define in (open-input-file file))
  (define out (open-output-file #:exists 'replace "output.txt"))

  (define (next) (read-char in))

  (define (iter-file word shifted?)
    (define char (next))
    (define last-idx
      (if (empty-word? word)
          0
          (- (string-length word) 1))
      )
    (define char-list (string->list word))
    (define last-char
      (if (empty-word? word)
          #f
          (car (drop char-list last-idx))
          )
      )
    (cond
      [(eq? char eof)
       (begin
         (if (not (empty-word? word))
             (fprintf out "~a" word)
             (void))
         (close-input-port in)
         (close-output-port out))
       ]
      [(and (equal? last-char #\-) (equal? char #\newline)) (iter-file (apply string (take char-list last-idx)) #t)]
      [(letter? char) (iter-file (string-append word (string char)) shifted?)]
      [(equal? char #\-) (iter-file (string-append word (string char)) shifted?)]
      [shifted?
       (fprintf out "~a\n" word)
       (skip-spaces in out char)
       (iter-file "" #f)
       ]
      [else (iter-file (string-append word (string char)) #f)]
      )
    )
  (iter-file "" #f)
  )

; 5

(define (translate original_file dictionary_file)
  (define original_port (open-input-file original_file))
  (define dictionary (map
                      (λ(k_v)
                        (define k_v_pair (string-split k_v " "))
                        (define key (car k_v_pair))
                        (define value (cadr k_v_pair))

                        (cons key value)
                        )
                      (port->lines (open-input-file dictionary_file))
                      )
    )
  (define out (open-output-file #:exists 'replace "output.txt"))

  (define (next) (read-char original_port))

  (define key caar)
  (define value cdar)
  (define (get-translation word dict)
    (cond
      [(empty? dict) word]
      [(equal? word (key dict)) (value dict)]
      [else (get-translation word (cdr dict))]
      )
    )

  (define (iter-file word)
    (define char (next))
    (cond
      [(eq? char eof)
       (begin
         (close-input-port original_port)
         (close-output-port out))
       ]
      [(letter? char) (iter-file (string-append word (string char)))]
      [else
       (begin (define written
                (if (empty-word? word)
                    char
                    (string-append (get-translation word dictionary) (string char))
                    )
                )
              (fprintf out "~a" written)
              (iter-file ""))
       ]
      )
    )
  (iter-file "")
  )