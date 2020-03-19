#lang racket


(define (leaf? tree)  ; проверяет что дерево — лист
  (andmap empty? (cdr tree))
  )

(define (monotree? tree)  ; проверяет, что у дерева всего один потомок
  (ormap empty? (cdr tree))
  )

(define left cadr)  ; вызывает левую ветку дерева
(define right caddr)  ; вызывает правую ветку дерева

; 1

(define (map-tree func tree)  ; map для дерева. func — функция, которую нужно применить к узлу, tree — дерево
  (cond
    [(empty? tree) tree]  ; если дерево пусто, возвращаем его
    [(leaf? tree) (list (func (car tree)) '() '())]  ; если дерево — лист, конструируем список из текущего узла, изменённого функцией func и двух пустых списков
    [else
     (list (func (car tree))  ; иначе, конструируем список из из текущего узла, изменённого функцией func,
           (map-tree func (left tree))  ; результата map-tree для левого дерева
           (map-tree func (right tree)))  ; и правого
     ]
    )
  )
  
  ; Пример — (map-tree add1 '(1 (1 () ()) (2 () ())))), результат — '(2 (2 () ()) (3 () ()))

; 2


(define (foldl-tree func result tree)  ; map для дерева. func — функция, которую нужно применить к узлу, result — то, куда записывается результат, tree — дерево
  (cond
    [(empty? tree) result]  ; дерево пусто, возвращаем неизменённый результат 
    [(leaf? tree) (func (car tree) result)]  ; дерево — лист, применяем к результату и текущему узлу функцию func
    [else  ; иначе,
     (let* ([current_result (func (car tree) result)] ; запоминаем результат вызова func для текущего узла и результата
            [left_result (foldl-tree func current_result (left tree))]  ; запоминаем результат для левого поддерева, помещая на место result current_result
            [right_result (foldl-tree func left_result (right tree))])  ; аналогично с правым поддеревом, но на место result идёт left_result
       right_result  ; возвращаем right_result
       )
     ]
    )
  )
  
  ; Пример — (foldl-tree cons '() '(1 (1 () ()) (2 () ())))), результат — '(2 1 1))

; 3

(define (get_next_tree tree)  ; возвращает следующее поддерево
  (if (monotree? tree)  ; если текущее дерево с одним потомком
      (if (empty? (left tree))  ; и пусто левое дерево
          (remove-monotrees (right tree))  ; пропускаем текущее поддерево tree, вызывая remove-monotrees для правого дерева
          (remove-monotrees (left tree))  ; иначе, пусто правое дерево и мы вызываем remove-monotrees для левого дерева
          )
      (list (car tree)  ; иначе, дерево имеет два потомка и мы возвращаем поддерево вида: текущий узел
            (remove-monotrees (left tree))  ; результат работы remove-monotrees для левого поддерева
            (remove-monotrees (right tree))  ; и правого
            )
      )
  )

(define (remove-monotrees tree)  ; основная функция
  (cond
    [(empty? tree) tree]  ; дерево пусто — возвращаем его
    [(leaf? tree) tree]  ; дерево лист — возвращаем его
    [else (get_next_tree tree)]  ; иначе, вызываем вспомогательную функцию get_next_tree (см.выше)
    )
  )
  
; Пример — (remove-monotrees '(1 (2 () (1 () ())) (1 () ()))), результат — '(1 (1 () ()) (1 () ()))

; 4

(define (get_sorted_children_roots root edges)  ; возвращает отсортированный список пар вида (текущий узел . его потомок)
  (sort (filter (λ(edge) (= (car edge) root)) edges)
        <
        #:key cdr  ; сортируем по потомку
        )
  )

(define left-child cdar)  ; возвращает значение левого потомка из списка пар
(define right-child cdadr)  ; возвращает значение правого потомка из списка пар

(define (build-tree edges)
  (define sorted-edges (sort edges < #:key car))  ; для начала сортируем по предкам начальный список пар
  (define (rec root edges)  ; основная рекурсивная функция. root — текущий корень поддерева (узел), edges — отсортированный список рёбер
    (define children (get_sorted_children_roots root edges))  ; запоминаем отсортированный список потомков для текущего корня
    (cond
      [(empty? children) (list root '() '())]  ; если потомков нет, то это лист и мы возвращаем дерево (текущий корень, два пустых списка)
      [else  ; дерево непусто, 
       (let* ([lroot (left-child children)]  ; значит у него есть как минимум левый потомок (левый, потому что children отсортированный)
              [ltree (rec lroot (dropf edges (λ(edge) (= (cdr edge) lroot))))]  ; запоминаем левое поддерево — вызов рекурсии, где корнем будет lroot, а edges — список рёбер, начиная с первой пары, в которой узлом будет lroot
              [rroot  ; запоминаем правый корень. Его может не быть, поэтому тут проверка
               (if (= (length children) 2)  ; если детей у текущего корня 2, то
                   (right-child children)  ; правый потомок существует, возвращаем его
                   '()  ; иначе, пустой список
                   )
               ]
              [rtree  ; запоминаем правое поддерево. rroot может не быть, проверяем
               (if (number? rroot)  ; если rroot — число (то есть узел есть)
                   (rec rroot (dropf edges (λ(edge) (= (cdr edge) rroot))))  ; rtree будет аналогично ltree
                   rroot  ; иначе, rroot будет пустым список, вернём его
                   )
               ]
              )
         (list root ltree rtree)  ; в итоге формируем поддерево — список из текущего корня, ltree, rtree
         )
       ]
      )
    )
  (if (empty? edges)  ; если список рёбер пуст,
      edges  ; возвращаем его, так как он итак пустой
      (rec (caar sorted-edges) sorted-edges)  ; иначе, вызываем рекурсию для 
      )
  )

; Пример — (build-tree '((1 . 2) (2 . 3) (2 . 4))), результат — '(1 (2 (3 () ()) (4 () ())) ())
