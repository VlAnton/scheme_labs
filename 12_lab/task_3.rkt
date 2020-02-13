#lang racket

(define (find_max_degree a b)  ; пример вызова — (find_max_degree 4 16), ответ 4
  (define min_number (min a b))  ; записываю min из a и b в переменную
  (define matching_sector (build-list (- min_number 1) (λ(x) (+ x 2))))  ; промежуток от 2 до минимума из a и b. Именно там будет ответ
  (define possible_nums  ; сужаем множество предполагаемых ответов, записываем его в переменную
    (foldr (λ(n res)
             (if (or (<= (* n n) min_number) (= n min_number))  ; если число в квадрате меньше минимума,
                 ; либо равно ему — оно является потенциальным ответом
                 (cons n res)  ; условие выполнено — записываем в результат
                 res  ; нет — оставляем результат неизменным
                 )
             ) '() matching_sector
               )
    )
  (foldl (λ(x res)  ; x — потенциальное число (из possible_nums), res — результат
           (define is_degree_a (natural? (inexact->exact (log a x))))  ; проверяем что a — натуральная степень x
           (define is_degree_b (natural? (inexact->exact (log b x))))  ; аналогично для b

           (cond
             [(not (boolean? res))  ; изначально res = #f, если нет — то это наш ответ
              (if (and is_degree_a is_degree_b)  ; но если a и b тоже являются степенями x
                  x  ; то возвращаем x, потому что possible_nums отсортировано по возрастанию и x > res, а нам нужен наибольший ответ
                  res  ; иначе, возвращаем res
                  )
              ]
             [(and is_degree_a is_degree_b) x]  ; если в результате #f, то проверяем что a и b являются степенями x. Если да, возвращаем его
             [else res]  ; иначе оставляем результат неизменным
             )
           ) #f possible_nums
             )
  )
  