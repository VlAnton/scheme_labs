#lang racket


; Общие

(define left cadr)  ; возвращает левую ветку дерева
(define right caddr)  ; возвращает правую ветку дерева

(define (leaf? tree)  ; проверяет, лист ли дерево
  (andmap empty? (cdr tree))
  )


; task_1

(define (get_current_result node left-node right-node)  ; возвращает результат на текущем шаге
  (cond
    [(and (number? left-node) (= (remainder node left-node) 0)) node]  ; если левый значение — число (то есть узел), то проверяем что остаток
    ; деления предыдущего узла на этот равен 0
    [(and (number? right-node) (= (remainder node right-node) 0)) node]  ; аналогично с правым
    [else #f]  ; иначе, возвращаем #f
    )
  )
  

(define (func tree)
  (cond
    [(or (empty? tree) (leaf? tree)) #f]  ; база рекурсии — текущее дерево пусто или лист, то #f
    [else  ; иначе
     (let* ([left-node  ; берём значение левого узла
             (if (empty? (left tree))
                 #f  ; если левое поддерево пусто, то узла нет и возвращаем #f
                 (car (left tree))  ; иначе, возвращаем узел
                 )
             ]
            [right-node  ; аналогично с правым
             (if (empty? (right tree))
                 #f
                 (car (right tree))
                 )
             ]
            [current_res (get_current_result (car tree) left-node right-node)]  ; получаем текущий результат
            [left_res (func (left tree))]  ; получаем результаты от левого
            [right_res (func (right tree))]  ; и правого деревьев
            )
       (cond
         [(number? current_res) current_res]  ; если текущий
         [(number? left_res) left_res]  ; или левый
         [(number? right_res) right_res]  ; или правый результаты являются числом, возвращаем их
         [else #f]  ; иначе, #f
         )
       )
     ]
    )
  )


; task_2

(define (symmetrical_btree tree)
  (cond
    [(or (empty? tree) (leaf? tree)) tree]  ; база рекурсии — дерево лист или пусто, возвращаем его
    [else  ; иначе, составляем на каждом шаге дерево вида
     (list
      (car tree)  ; текущая голова дерева
      (symmetrical_btree (right tree))  ; поддерево для правого дерева
      (symmetrical_btree (left tree))  ; поддерево для левого дерева
      )
     ]
    )
  )

; task_3

(define (count_left_branches tree)
  (cond
    [(or (empty? tree) (leaf? tree)) 0]  ; база рекурсии — дерево пусто или лист, возвращаем 0
    [else
     (let ([summand  ; задаём значение слагаемому
            (if (not (empty? (left tree)))  ; если левое поддерево непусто, то 1, иначе 0
                1
                0
                )
            ]
           )
       (+ summand (count_left_branches (left tree)) (count_left_branches (right tree)))  ; и прибавляем слагаемое к вызовам
       ; от правого и левого деревьев
       )
     ]
    )
  )


; 4_task

(define (min_leaf tree)
  (cond
    [(empty? tree) +inf.0]  ; база рекурсии — дерево пусто, возвращаем -бесконечность
    [(leaf? tree) (car tree)]  ; дерево — лист, возвращаем его узел
    [else
     (min (min_leaf (left tree)) (min_leaf (right tree)))  ; возвращаем минимум вызовов от левых и правых поддеревьев
     ]
    )
  )


; 5_task

(define (!= . elems)  ; написал для удобства )
  (not (apply eq? elems))
  )


(define (generate_list lst)  ; генерирует итоговый результат
  (define sorted-lst (sort lst < #:key cdr))  ; сортируем список по индексу
  (define nodes (append (map car sorted-lst) (list -inf.0)))  ; берём оттуда список узлов + -inf.0, чтобы списки были одинаковой длины
  (define i_s_original (map cdr sorted-lst))  ; список индексов
  (define i_s_cut (append (cdr i_s_original) (list -inf.0)))  ; обрезаем список индексов, чтобы можно было сравнивать во время итерации

  (reverse  ; результат будет не в правильном порядке, поэтому ревёрсим его
   (cdr  ; обрезаем -inf.0
    (foldl
     (λ(node i i1 res)  ; node — текущий узел, i — его индекс, i1 — индекс текущего узла
       (cond
         [(eq? i i1) (cons (cons node (car res)) (cdr res))]  ; если индексы равны, то прибавляем node к списку с такими же индексами
         [else (cons (list node) res)] ; иначе, создадим в нашем списке списков список для нового индекса
         )
       ) (list (list (car nodes))) (cdr nodes) i_s_cut i_s_original  ; на входе обрезаем nodes, а в результат помещаем первый его элемент
         )
    )
   )
  )


(define (tree->list tree)
  (define (iter-tree tree result i)  ; итерация по дереву
    ; на каждом шаге увеличиваем i и к результату добавляем пару вида (текущий узел, его индекс)
    (let* ([ltree (left tree)]  ; левое поддерево
           [rtree (right tree)]  ; правое
           )
      (if (empty? rtree)  ; если правое пусто
          (if (empty? ltree)  ; и левое пусто
              result  ; возвращаем результат
              (iter-tree ltree (append result (list (cons (car ltree) i))) (+ i 1)))  ; иначе, итерируемся по левому
          (if (empty? ltree)  ; если правое непусто, а левое пусто
              (iter-tree rtree (append result (list (cons (car rtree) i))) (+ i 1))  ; итерируемся по правому
              (append  ; иначе, они оба не пусты и итерируемся по обоим
               (iter-tree ltree (append result (list (cons (car ltree) i))) (+ i 1))
               (iter-tree rtree (append result (list (cons (car rtree) i))) (+ i 1))
               )
              )
          )
      )
    )
  (if (empty? tree)  ; если дерево пустое, то по условию #f
      #f
      (cons  ; иначе, генерируем список
       (list (car tree))  ; голову приписываем сразу
       (generate_list (iter-tree tree '() 2))
       )
      )
  )