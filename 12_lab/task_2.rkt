#lang racket


(define (get_triple lst)  ; Пример вызова — (get_triple '(1 2 3)), ответ (1 2 3)
  (define idxs (build-list (length lst) values))  ; задаю заранее список индексов
  ; способ решения — перебор всех троек чисел из списка в надежду найти нужную пару
  (foldl (λ(i idx_i res_i)  ; i — число, idx_i — его индекс в списке, res_i — результат работы всей функции
           (cond
             [(not (boolean? res_i)) res_i]  ; изначально результат в каждом цикле равен #f, так как предполагаем что таких троек чисел нет
             ;  #f == False — boolean значение, а тройка чисел — нет. Если res_i не boolean, то значит мы нашли ответ и дальше возвращаем только его
             [else
              (foldl (λ(j idx_j res_j)  ; j — число, idx_j — его индекс в списке, res_j — результат работы вложенного foldl функции
                       (cond
                         [(not (boolean? res_j)) res_j]  ; аналогично с res_j
                         [(= idx_i idx_j) res_i]  ; idx_i == idx_j => это одно и то же число, а мы такое не рассматриваем
                         [else  ; иначе, ещё один внутренний foldl
                          (foldl (λ(k idx_k res_k)  ; аннотация аналогична предыдущим foldl
                                   (cond
                                     [(not (boolean? res_k)) res_k]  ; точно также возвращаем res_k, если оно не bool
                                     [(or (= idx_i idx_k) (= idx_j idx_k)) #f]  ; также проверяем что индексы не равны
                                     [(= (+ i j) k) (list i j k)]  ; если i + j = k, то возвращаем '(i j k)
                                     [else #f]  ; если нет, то оставляем #f
                                     )
                                   ) #f lst idxs  ; что изначально результат #f на всех foldl
                                     )
                          ]
                         )
                       ) #f lst idxs  ; что изначально результат #f на всех foldl
                         )
              ]
             )
           ) #f lst idxs  ; что изначально результат #f на всех foldl
             )
  )