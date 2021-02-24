#lang racket


; 1 — производная многочлена

(define (derivative polynom)
  (define current-degree (- (length polynom) 1))

  (if (empty? (cdr polynom))
      '()
      (cons (* current-degree (car polynom)) (derivative (cdr polynom))))
  )


; 2 — сумма многочленов

(define (add-zeros polynoms)
  (define max-length (foldl (λ (polynom current-max)
                              (define poly-len (length polynom))
                              (if (> poly-len current-max)
                                  poly-len
                                  current-max)) 0 polynoms))
  (map (λ(polynom)
         (define polynom-extension (make-list (- max-length (length polynom)) 0))
         (append polynom-extension polynom)) polynoms)
  )

(define (sum-of-polynoms . polynoms)
  (apply map + (add-zeros polynoms))
  )


; 3 — умножение списков

; список индексов по убыванию от degree до 0
(define (get-degrees degree)
  (build-list degree (λ(x) (- degree 1 x)))
  )

(define (multiply p1 p2)
  (define degrees (get-degrees (length p1)))

  (apply sum-of-polynoms
         (map (λ(x degree)
                (append (map (λ(y)(* x y)) p2) (make-list degree 0))) p1 degrees))
  )

(define (multiply-polynoms . polynoms)
  (if (empty? (cdr polynoms))
      (car polynoms)
      (multiply (car polynoms) (apply multiply-polynoms (cdr polynoms))))
  )


; 4

(define (list->number lst)
  (foldl (λ(x res) (+ (* res 10) x)) 0 lst)
  )

(define (number->list n)
  (define (iter x res)
    (if (= x 0)
        res
        (iter (quotient x 10) (cons (remainder x 10) res)))
    )
  (iter n '())
  )

(define (sum-in-kth-system k . numbers)
  (define num-lists (map number->list numbers))
  (define sum (apply sum-of-polynoms num-lists))
  (define result (foldr (λ(num res)  ; num — текущий номер, res — результат в формате ((текущая сумма) . остаток)
                          (define r (foldr (λ(x result)
                                             (define rem (cdr result))
                                             (define new-x (+ x rem))
                                             (if (>= new-x k)
                                                 (cons (cons (- new-x k) (car result)) 1)
                                                 (cons (cons new-x (car result)) 0))) (cons '() (cdr res)) (sum-of-polynoms (car res) num)))
                          (if (zero? (cdr r))
                              (cons (car r) 0)
                              (cons (cons (cdr r) (car r)) 0))
                          ) (cons '() 0) num-lists))
  (define is-correct-data (andmap (λ(num-list)
                                    (andmap (λ(x) (< x k)) num-list)) num-lists)
    ) ; флаг, проверяющий что в numbers нет цифр, больших k (такое может быть, если был некорректный ввод)
  (define rem (cdr result))  ; остаток
  (cond
    [(not is-correct-data) #f]  ; если ввод некорректный — #f
    [(= rem 0) (list->number (car result))]  ; превращаем результат в число и возвращаем
    [else (list->number (cons rem (car result)))])  ; иначе добавлем единичку в начало результата, превращаем в число и возвращаем
  )


; 5

; функкция возвращает значение f в точке х, где f — многочлен

(define (get-value f x)
  (foldl (λ(coef degree res)
           (+ res (* coef (expt x degree)))) 0 f (get-degrees (length f)))
  )

; реализация самого метода
(define (newton-method x0 f)
  (define df (derivative f))
  (define eps 0.001)

  (define (iter xn)
    (define xn+1 (- xn (/ (get-value f xn) (get-value df xn))))
    (if (< (abs (- xn+1 xn)) eps)  ; если |xn+1 - xn| < eps
        xn+1
        (iter xn+1))
    )

  (iter x0)
  )
; в качестве примера можешь взять первый пример отсюда http://www.mathprofi.ru/metod_kasatelnyh.html
