#lang racket


; Вспомогательные функции

(define (leaf? subtree)
  (andmap empty? (cdr subtree))
  )


; 1

(define (get_triplets N_list)
  (define triplets_list  ; заводим переменную, являющуюся списком вида: '((список чисел с остатком 0) (с остатком 1) (с остатком 2))
    (foldl  ; получаем её значение с помощью следующего foldl
     (λ(x res)  ; x — число из N_list, res — результирующий список вида, описанного выше
       (define 0_part (car res))  ; (список чисел с остатком 0)
       (define 1_part (cadr res))  ; (список чисел с остатком 1)
       (define 2_part (caddr res))  ; (список чисел с остатком 2)

       (cond
         [(= (remainder x 3) 0) (list (cons x 0_part) 1_part 2_part)]  ; если остаток от x / 3 == 0, добавляем x к 0_part 
         [(= (remainder x 3) 1) (list 0_part (cons x 1_part) 2_part)]  ; аналогично с 1
         [(= (remainder x 3) 2) (list 0_part 1_part (cons x 2_part))]  ; аналогично с 2
         [else res]  ; эта строчка не нужна, написал просто так
         )
       ) '(() () ()) N_list  ; на вход подаём '(()()()) — сюда будет записываться результат, и N_list
         )
    )
  (if (ormap empty? triplets_list)  ; если хоть один из списков в triplets_list пуст, то
      #f  ; мы не можем составить тройки чисел и возвращаем #f
      (apply cartesian-product triplets_list)  ; иначе, применяем функцию cartesian-product (декартово произведение)
      ; к спискам из triplets_list с помощью apply
      )
  )


; 2

(define (last_element lst)
  (if (empty? (cdr lst))
      (car lst)
      (last_element (cdr lst))
      )
  )


(define (get_full_path directories)
  (define (get_path directories result)  ; тут происходит поиск пути
    (define list-head  ; записываю голову списка заранее, потому что мы в конце доходим до момента,
      ; когда список пуст и мы не можем взять car от списка, а он нам нужен строчкой ниже
      (if (empty? directories)  ; если директория пустая,
          #f  ; возвращаем #f
          (car directories)  ; иначе, голову списка
          )
      )
    (define idxs (indexes-of result list-head))  ; получаем список индексов последнего вхождения элемента в результирующий список,
    ; либо #f, если его в результате нет
    (define idx
      (if (empty? idxs)
          #f
          (last_element idxs)
          )
      )
    
    (cond
      [(empty? directories) result]  ; если дошли до конца списка директорий — возвращаем результат
      [(and (number? idx) (= (- (length result) idx) 2))  ; если idx существует и длина результата минус idx == 2 (по условию это недопустимо)
       (get_path (cdr directories) (append (take result idx) (list list-head)))  ; то мы обрезаем всё,
       ; что было между старым индексом и новым и ставим добавляем к результату этот элемент
       ]
      [else (get_path (cdr directories) (append result (list list-head)))]  ; иначе, всегда добавляем элемент к результату
      )
    )

  (define (get_starting_point directories)  ; мы идём по списку директорий, пока не найдём C:\
    (cond
      [(empty? directories) #f]  ; если список директорий закончился, то мы не нашли стартовую точку пути => возвращаем #f
      [(eq? (car directories) "C:\\") (get_path directories '())]  ; если нашли, то запускаем поиск пути
      [else (get_starting_point (cdr directories))]  ; иначе, продолжаем поиск стартовой точки
      )
    )

  (get_starting_point directories)  ; запускается всё на этом моменте
  )


; 3

(define (max_in_tree tree)  ; функция, находящая максимум в дереве
  (cond
    [(empty? tree) -inf.0]  ; база рекурсии — дерево пустое, возвращаем - бесконечность (максимально маленькое число, чтобы не сломать алгоритм)
    [(leaf? tree) (car tree)]  ; если текущее дерево — лист, возвращаем голову дерево, то есть узел
    [else  ; иначе,
     (max  ; применяем рекурсивно max к 
      (car tree)  ; текущему узлу
      (max_in_tree (cadr tree))  ; вызову max_in_tree по левому поддереву
      (max_in_tree (caddr tree))  ; вызову max_in_tree по правому поддереву
      ; дело в том, что вызовы по левому и правому поддереву также будут возвращать узлы и в итоге мы найдём max среди всех узлов
      )
     ]
    )
  )


(define (get_summand node max_node)  ; функция, чтобы определить, что добавлять к результату на текущем вызове рекурсии
  (if (= (car node) max_node)  ; если текущий узел равен максимальному, то 1, иначе 0
      1
      0
      )
  )

(define (count_maxes tree)
  (define max_node (max_in_tree tree))  ; запоминаем максимальный узел
  (define (count_recursion tree)  ; функция для рекурсивного подсчёта максимумов в дереве
    (cond
      [(empty? tree) 0]  ; база рекурсии — дерево пустое, возвращаем 0
      [else
       (let ([counter (get_summand tree max_node)])  ; с помощью let заводим переменную, содержащее 1 или 0 в зависимости от того,
         ; равен ли текущий узел максимальному
        (+
         counter
         (count_recursion (cadr tree))  ; рекурсивно добавляем к рекурсивному вызову count_recursion от левого поддерева
         (count_recursion (caddr tree))  ; и от правого
         )
        )
       ]
      )
    )
  (count_recursion tree)  ; вызов рекурсии
  )


; 4

(define (count_leaves tree)  ; подсчёт листов дерева
  (cond
    [(empty? tree) 0]  ; база рекурсии — дерево пустое (возвращаем 0)
    [(leaf? tree) 1]  ; если встретили лист — возвращаем 1
    [else  ; иначе,
     (+  ; складываем
      (count_leaves (cadr tree))  ; рекурсивные вызовы count_leaves от левого
      (count_leaves (caddr tree))  ; и правого поддеревьев
      )
     ]
    )
  )


; 5

(define (binary? tree)  ; проверка дерева на бинарность
  (if (or (empty? tree) (leaf? tree))  ; база рекурсии — если дерево пустое, либо лист (по условию задачи это #t)
      #t  ; тогда #t
      (and (= (length tree) 3) (binary? (cadr tree)) (binary? (caddr tree)))  ; иначе, рекурсивно с помощью and получаем предикат,
      ; проверяющий что длина текущего дерева и его поддеревьев равна 3 (то есть узел и два потомка)
      )
  )
