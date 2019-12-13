#lang scheme

; Вспомогательные функции

(define (min-or-max-in-row-list func row)
  (foldl (λ(idx x res)
           (define el (caar res))  ; текущий max/min элемент
           (define el_idx (cdar res))
           (define new_el (func el x))  ; вычисляем новый max/min элемент

           (cond
             [(and (= x el) (not (= el_idx idx))) (cons (cons new_el idx) res)]  ; если текущий элемент равен старому максимальному и их индексы не равны (это условие-костыль,
                                                                                 ; чтобы самый первый элемент не добавлялся дважды), то добавляем новую пару к результирующему списку
             [(= new_el x) (cons (cons new_el idx) '())]  ; если текущий элемент больше старого максимального, то создаём новый список с новым максимальным элементом
             [else res]  ; иначе, не изменяем список
             )
           )
         (cons (cons (car row) 0) '())  ; нужно начальное значение, пусть будет список с одним элементом – (первый элемент строки . его индекс)
         (build-list (length row) values)  ; список индексов
         row  ; строка (:
         )
  )


(define (min-or-max-in-row func row)  ; функция, возвращающая минимум или максимум для строки
  (apply func row)
  )


(define (columns-for-idxs idxs matr)  ; возвращает список столбцов матрицы для заданных индексов
  (map (λ(idx) (column-for-idx idx matr)) idxs)  ; на каждом шаге для индекса создаём с помощью column-for-idx список столбцов
  )


(define (column-for-idx idx matr)  ; возвращает столбец матрицы для заданного индекса
  (map (λ(row) (list-ref row idx)) matr)  ; на каждом шаге у строки с помощью list-ref берём элемент по заданному индексу
  )


; Версия, которая точно корректно работает в матрице с разными числами. То-есть когда максимум/минимум в столбце/строке гарантировано один.

(define (task3 matr) 
  (foldl (λ(row res)  ; аргументы на каждом шаге:
                                            ; row — строка матрицы
                                            ; res — результирующий список, в который добавляются седловые точки
           (define min_in_row_with_idx (min-or-max-in-row min row))  ; пара в виде (минимальный элемент в строке . его индекс)
           (define min_in_row (car min_in_row_with_idx))  ; минимальный элемент в строке
           (define min_idx (cdr min_in_row_with_idx))  ; его индекс

           (define column (column-for-idx min_idx matr))  ; столбик по индексу минимального элемента
           (define max_in_column_with_idx (min-or-max-in-row max column))  ; пара в виде (максимальный элемент в столбике . его индекс)
           (define max_in_column (car max_in_column_with_idx))  ; максимальный элемент в столбике
           (define max_idx (cdr max_in_column_with_idx))  ; его индекс

           (if (= min_in_row max_in_column)  ; сравниваем максимальный элемент в столбце и минимальный в строке
               (cons min_in_row res)  ; если они равны, то это седловая точка и мы её добавляем функцией cons
               res  ; иначе, оставляем результат таким же
               )
           ) '() matr  ; в качестве аргументов в foldl подаём пустой список и матрицу
             )
  )


; Версия, которая работает ещё и с повторяющимися max/min элементами строки/столбца

(define (task3-list matr)
  (define idxs (build-list (length matr) values))  ; список индексов строк

  (foldl (λ(row_idx row res)  ; аргументы на каждом шаге:
                                            ; row_idx – индекс строки на текущем шаге
                                            ; x – текущая строка
                                            ; res – результирующий список седловых точек
           (define mins_in_row_with_idxs (min-or-max-in-row-list min row))  ; список минимальных точек в текущей строке в формате списка пар вида (минимальный элемент в строке . его индекс)
           (define saddle_points (foldl (λ(min_row_data dum)  ; вычисляем седловые точки в текущей строке. Аргументы на каждом шаге:
                                                                                                                       ; min_row_data – информация о текущем минимальном элементе
                                                                                                                       ; dum – результируюзий список
                                         (define min_in_row (car min_row_data))  ; текущий минимальный элемент в строке
                                         (define min_idx (cdr min_row_data))  ; его индекс

                                         (define column (column-for-idx min_idx matr))  ; столбец матрицы по индексу текущего минимального элемента строки
                                         (define maxs_in_column_with_idx (min-or-max-in-row-list max column))  ; список максимальных элементов в формате, аналогичном списку минимальных
                                         (define inner_saddle_points (foldl (λ(max_column_data points)  ; проходим по списку максимумов и сравниваем с соответствующими минимумами
                                                                             (define max_in_column (car max_column_data))  ; максимальный элемент в столбце
                                                                             (define max_idx (cdr max_column_data))  ; его индекс

                                                                             (if (and (= max_in_column min_in_row) (= max_idx row_idx))  ; сравниваем максимальный и минимальный элемент и их индексы
                                                                                 (cons min_in_row points)  ; если они совпадают, добавляем в результирующий список
                                                                                 points  ; иначе, ничего не делаем
                                                                                 )
                                                                             ) '() maxs_in_column_with_idx
                                                                               )
                                           )

                                         (if (empty? inner_saddle_points)  ; проверяем список седловых точек для этой строки на пустоту
                                             dum  ; если список пуст, то не меняем результат
                                             (append inner_saddle_points dum)  ; иначе, присоединяем его к результату
                                             )
                      
                                         ) '() mins_in_row_with_idxs
                                           )
             )
           (if (empty? saddle_points)  ; тут аналогично
               res
               (append saddle_points res)
               )
           ) '() idxs matr)
  )