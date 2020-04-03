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
         (close-input-port in)
         (close-output-port out))
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
      [(eq? byte eof)
       (begin
         (close-input-port in)
         (close-output-port out))
       ]
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


       