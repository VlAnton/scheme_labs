#lang racket


; Вспомогательные функции

; функция для получения максимального по длине полинома
(define (get_max_polynom polynoms)
  (foldl
   (λ(polynom current_max)  ; текущий полином и результат
     (if (> (length polynom) (length current_max))  ; если длина текущего полинома больше текущего максимума, то
         polynom  ; возвращаем текущий полином
         current_max  ; иначе — оставляем результат неизменным
         )
     ) '() polynoms
       )
  )

; возвращаем список расширенных полиномов. Расширить полином в данном контексте == добавить слева нуликов
; до длины максимального полинома в списке. Нужно для суммирования полиномов
(define (get_extend_polynoms polynoms)
  (define max_polynom (get_max_polynom polynoms))  ; находим max_polynom, записываем в переменнуюю
  (map
   (λ(polynom)
     (define polynom_extension_length (- (length max_polynom) (length polynom)))  ; берём длину расширения (длина max - длина текущего)
     (define polynom_extension (make-list polynom_extension_length 0))  ; создаём расширение списка - список из нулей длины polynom_extension_length
     (append polynom_extension polynom)  ; добавляем слева к текущему элементу расширение
     ) polynoms
       )
  )

; превращение списка цифр в число
(define (list->number lst)
  (foldl (λ(x res) (+ (* res 10) x)) 0 lst)  ; на каждом шаге результат умножаем на 10 и прибаавляем x
  )

; превращение числа в список цифр
(define (number->list n)
  (define (iter x res)
    (if (= x 0)  ; если x == 0, то
        res  ; заканчиваем итерацию и возвращаем список цифр
        (iter (quotient x 10) (cons (remainder x 10) res))  ; иначе на каждом шаге уменьшаем число на разряд,
        ; а текущий разряд добавляем в результирующий список
        )
    )
  (iter n '())  ; на входе — число и пустой список, в который будут накапливаться цифры
  )

; список индексов по убыванию от degree до 0
(define (reversed_idxs degree)  ; degree — неотрицательное число
  (build-list degree (λ(x) (- degree 1 x)))  ; создаём список длины degree, где каждый x равен degree - 1 - x
  )

; Задачи

; 1 — производная многочлена
(define (differentiate_polyndrom polyndrom)
  (define (rec polyndrom)  ; делаем рекурсивно
    (define current_degree (- (length polyndrom) 1))  ; степень текущей головы списка/полинома

    (if (empty? (cdr polyndrom))  ; база рекурсии — если дошли до конца списка, возвращаем пустой список
        '()
        (cons (* current_degree (car polyndrom)) (rec (cdr polyndrom)))  ; иначе — прибавляем cons'ом на каждом шаге к результату элемент вида:
        ; текущая степень * текущий элемент (это реализация n * x^(n-1))
        )
    )
  (rec polyndrom)
  )


; 2 — сумма многочленов
(define (sum_of_polynoms . polynoms) ; ". polynoms" означает, что в функцию может подаваться сколько угодно параметров
  (define extended_polynoms (get_extend_polynoms polynoms))  ; составляем список расщиренных до максимального по длине полиномов

  (apply map + extended_polynoms)  ; (map + lst1 lst2 lst3...) возвращает список,
  ; каждый элемент которого равен сумме соответствующих элементов из lst1/2/3/... . extended_polynoms будет список вида '(lst1 lst2 ...),
  ; поэтому чтобы применить map, используем apply. Он применит map уже к элементам списка
  )

; 3 — умножение списков
(define (multiply p1 p2)  ; реализация умножения двух многочленов p1 и p2
  (define degrees (reversed_idxs (length p1)))  ;  получаем список степеней

  (apply
   sum_of_polynoms  ; применяем функцию суммы полиномов
   (map
   ; напомню как выглядит умножение двух полиномов:
   ; (ax^2 + b)(cx + d) = (acx^3 + adx^2) + (bcx + bd). В этом map мы составляем список слагаемых из правой части
    (λ(x degree)  ; x — текущий кофициент из p1 (например, "a" из примера выше), degree — степень икса при коэф (например, 2 при "a")
      (define ext (make-list degree 0))  ; составляем расширение полинома длины degree из нулей
      (append (map (λ(y)(* x y)) p2) ext)  ; добавляем его справа, предварительно умножив x на все элементы p2 с помощью map
      )
    p1 degrees
    )
   )
  )

; рекурсивное умножение неограниченного числа многочленов
(define (multiply_polynoms . polynoms)
  (if (empty? (cdr polynoms))  ; база рекурсии — если в списке остался один многочлен
      (car polynoms)  ; возвращаем его
      (multiply  ; иначе, умножаем
       (car polynoms)  ; текущий многочлен
       (apply multiply_polynoms (cdr polynoms))  ; на рекурсивный вызов multiply_polynoms, куда с помощью apply подаём оставшиеся полиномы
       )
      )
  )


; 4

; сумма чисел в k-й системе счисления
(define (sum_in_kth_system k . numbers)  ; k — номер системы счисления, . numbers — числа в неограниченном количестве
  (define num_lists (map number->list numbers))  ; превращаем все числа в списки цифр
  (define sum (apply sum_of_polynoms num_lists))  ; заранее складываем их
  (define is_correct_data  ; проверка что в num_lists нет цифр, больших k (такое может быть, если был некорректный ввод)
    (foldl
     (λ(num_list res)
      (and (andmap (λ(x) (< x k)) num_list) res))  ; 
     #t
     num_lists
     )
    )

  (define result
    (foldr
    ; напоминаю про сложение чисел в kй степени:
    ; если сумма текущих разрядов превышает k, то запоминаем 1, переносим его в старший разряд, а в текущий записываем сумму разрядов - k
    ; у нас сумма уже сгенерирована, данный foldr обрабатывает остатки
     (λ(x res)  ; x — текущий разряд, res — результат в формате ((текущая сумма) . остаток)
       (define rem (cdr res))  ; остаток
       (define new_x (+ x rem))  ; сразу прибавляем остаток к текущему разряду, получаем новое значение разряда
       (define result (car res))  ; результат

       (cond
         [(>= new_x k) (cons (cons (- new_x k) result) 1)]  ; если новое значение разряда >= k, то новым разрядом будет (new_x - k),
         ; а в остатке будет 1
         [else (cons (cons new_x result) 0)]  ; иначе, остаток равен 0
         )
       ) (cons '() 0) sum
         )
    )
  (define rem (cdr result))  ; остаток
  (cond
    [(not is_correct_data) #f]  ; если ввод некорректный — #f
    [else
     (if (= rem 0)  ; если остаток равен 0, то
        (list->number (car result))  ; превращаем результат в число и возвращаем
        (list->number (cons rem (car result)))  ; иначе добавлем единичку в начало результата, превращаем в число и возвращаем
        )
     ]
    )
  )

; 5 — метод ньютона  (тут просто реализовать алгоритм, глянь в файле лабы)

; функкция возвращает значение f в точке х, где f — многочлен
(define (get_value f x)
  (define degrees (reversed_idxs (length f)))  ; степени x
  (foldl
   (λ(coef degree res)  ; degree — степень, coef — коэфициет при данной степени x
     (+ res (* coef (expt x degree)))  ; прибавляем к результату coef * x^degree
     ) 0 f degrees
       )
  )

; реализация самого метода
(define (newton_method x0 f)
  (define df (differentiate_polyndrom f))  ; производная f
  (define eps 0.001)  ; погрешность (можешь указать любую)

  (define (iter xn)
    (define xn+1 (- xn (/ (get_value f xn) (get_value df xn))))  ; xn+1 = xn - f(xn) / f'(xn)
    (if (< (abs (- xn+1 xn)) eps)  ; если |xn+1 - xn| < eps
        xn+1  ; возвращаем xn+1
        (iter xn+1)  ; иначе, следующая итерация
        )
  )

  (iter x0)  ; запускаем итерацию с начальным приближением
  )
; в качестве примера можешь взять первый пример отсюда http://www.mathprofi.ru/metod_kasatelnyh.html
