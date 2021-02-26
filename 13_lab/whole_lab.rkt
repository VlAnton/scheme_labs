#lang racket


; 1

(define (derivative polynom)
  (define current-degree (- (length polynom) 1))

  (if (empty? (cdr polynom))
      '()
      (cons (* current-degree (car polynom)) (derivative (cdr polynom))))
  )


; 2

(define (sum-of-polynoms . polynoms)
  (define max-length (apply max (map length polynoms)))
  (define extended-polynoms (map (λ(polynom)
                                   (append (make-list (- max-length (length polynom)) 0) polynom)) polynoms))
  (apply map + extended-polynoms)
  )


; 3

(define (multiply-polynoms . polynoms)
  (define (multiply-binary p1 p2)
    (define degree (length p1))
    (define degrees (build-list degree (λ(x) (- degree 1 x))))

    (apply sum-of-polynoms
           (map (λ(x degree)
                  (append (map (λ(y)(* x y)) p2) (make-list degree 0))) p1 degrees))
    )

  (apply multiply-binary polynoms)
  )


; 4

(define (list->number lst x)
  (foldl (λ(n res) (+ (* res x) n)) 0 lst))

(define (number->list n)
  (define (iter x res)
    (if (= x 0)
        res
        (iter (quotient x 10) (cons (remainder x 10) res)))
    )
  (iter n '()))

(define (sum-in-kth-system k . numbers)
  (define num-lists (map number->list numbers))
  (define result (foldr (λ(num res)  ; num — текущий номер, res — результат в формате ((текущая сумма) . остаток)
                          (define r (foldr (λ(x result)
                                             (define rem (cdr result))
                                             (define new-x (+ x rem))
                                             (if (>= new-x k)
                                                 (cons (cons (- new-x k) (car result)) 1)
                                                 (cons (cons new-x (car result)) 0))) (cons '() (cdr res)) (sum-of-polynoms (car res) num)))
                          (if (zero? (cdr r))
                              r
                              (cons (cons 1 (car r)) 0))
                          ) (cons '() 0) num-lists))
  (define rem (cdr result))
  (if (zero? rem)
      (list->number (car result) 10)
      (list->number (cons rem (car result)) 10))
  )


; 5

(define (newton-method x0 f)
  (define df (derivative f))
  (define eps 0.001)

  (define (iter xn)
    (define xn+1 (- xn (/ (list->number f xn) (list->number df xn))))
    (if (< (abs (- xn+1 xn)) eps)  ; если |xn+1 - xn| < eps
        xn+1
        (iter xn+1)))

  (iter x0)
  )
; в качестве примера можешь взять первый пример отсюда http://www.mathprofi.ru/metod_kasatelnyh.html
