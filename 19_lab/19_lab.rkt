#lang racket


; 1

(define (get-num-val byte)
  (- byte 48)
  )

(define (digit? byte)
  (define num_val (get-num-val byte))
  (and (>= num_val 0) (< num_val 10))
  )

(define (percent? byte)
  (= byte 37)
  )

(define (close-ports in out)
  (close-input-port in)
  (close-output-port out)
  )

(define (format filename . words)
  (define in (open-input-file filename))
  (define out (open-output-file #:exists 'replace "output.txt"))

  (define (next) (read-byte in))

  (define (insert-word? str words)
    (define byte (next))
    (define word
      (if (= (length words) 0)
          #f
          (car words)
          )
      )

    (cond
      [(equal? byte eof)
       (begin
         (fprintf out "~a" str)
         (close-ports in out))
       ]
      [(digit? byte)
       (begin
         (define new (string-append str (string (integer->char byte))))
         (insert-word? new words))
       ]
      [(percent? byte)
       (cond
         [(= (string-length str) 1)
          (begin
            (fprintf out "~a" str)
            (iter-file words))
           ]
         [(boolean? word)
          (begin
            (fprintf out "~a" str)
            (fprintf out "~a" (string (integer->char byte)))
            (iter-file words))
          ]
          [else
           (begin
             (fprintf out "~a" word)
             (iter-file (cdr words)))
           ]
          )
       ]
      [else
       (begin
         (fprintf out "~a" (string (integer->char byte)))
         (iter-file words)
         )
       ]
      )
    )

  (define (iter-file words)
    (define byte (next))

    (cond
      [(eq? byte eof) (close-ports in out)]
      [(percent? byte) (insert-word? "%" words)]
      [else
       (begin
         (fprintf out "~a" (string (integer->char byte)))
         (iter-file words))
       ]
      )
    )

  (iter-file words)
  )


; 2


(define (number->list n)
  (if (< n 10)
      (list n)
      (append (number->list (quotient n 10)) (list (remainder n 10)))
      )
  )

(define (ascending? number)
  (define digits (number->list number))
  (define (rec digit digits)
    (if (empty? digits)
        #t
        (and (<= digit (car digits)) (rec (car digits) (cdr digits)))
        )
    )
  (rec (car digits) (cdr digits))
  )

(define (get_sums filename)
  (define in (open-input-file filename))
  (define out (open-output-file #:exists 'replace "output.txt"))

  (define (next) (read-line in))
  (define (get_sum)
    (define line (next))
    (if (equal? line eof)
        eof
        (begin
          (foldl + 0 (map string->number (string-split line "+")))
          )
        )
    )
  (define (insert number lst)
    (define (rec lst)
      (if (empty? lst)
          (list number)
          (cond
            [(>= number (car lst)) (cons number lst)]
            [(< number (car lst)) (cons (car lst)
                                        (rec (cdr lst)))]
            )
          )
      )
    (iter-file (rec lst))
  )
  (define (write-out sums)
    (for-each (λ(line) (fprintf out "~a\n" line)) sums)
    )

  (define (iter-file sums)
    (define sum (get_sum))
    (cond
      [(equal? sum eof)
       (begin
         (write-out sums)
         (close-ports in out))
       ]
      [(ascending? (abs sum)) (insert sum sums)]
      [else (iter-file sums)]
      )
    )
  (iter-file '())
  )


; 3

(define define_list (string->list "define"))

(define (eof? char) (equal? char eof))
(define (open-bracket? char) (equal? char #\())
(define (close-bracket? char) (equal? char #\)))
(define (d? char) (equal? char #\d))
(define (define? str) (equal? str "define"))

(define (member? element lst)
  (list? (member element lst)))


(define (count-functions filename)
  (define in (open-input-file filename))
  (define (next) (read-char in))

  (define (iter-file count)
    (define char (next))

    (define (write-count)
      (begin
        (display count)
        (close-input-port in))
      )

    (define (stage-four)
      (define char (next))
      (cond
        [(eof? char) (write-count)]
        [(close-bracket? char) (iter-file (+ count 1))]
        [else (stage-four)]
        )
      )

    (define (stage-three)
      (define char (next))
      (cond
        [(eof? char) (write-count)]
        [(open-bracket? char) (iter-file count)]
        [(close-bracket? char) (stage-four)]
        [else (stage-three)]
        )
      )
    (define (stage-two str)
      (define char (next))
      (cond
        [(eof? char) (write-count)]
        [(member? char define_list) (stage-two (string-append str (string char)))]
        [else
         (if (define? str)
             (stage-one #t)
             (iter-file count))
         ]
        )
      )

    (define (stage-one in?)
      (define char (next))
      (cond
        [(eof? char) (write-count)]
        [(and (not in?) (d? char)) (stage-two "d")]
        [(and in? (open-bracket? char)) (stage-three)]
        [else (stage-one in?)]
        )
      )
    (cond
      [(eof? char) (write-count)]
      [(open-bracket? char) (stage-one #f)]
      [else (iter-file count)]
      )
    )
  (iter-file 0)
  )