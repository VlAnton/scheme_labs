#lang racket


; Общие

(define (left tree)  ; возвращает левую ветку дерева
  (cadr tree)
  )

(define (right tree)  ; возвращает правую ветку дерева
  (caddr tree)
  )

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