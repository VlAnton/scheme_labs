#lang racket


(define (split str delim)
  (define (clean_word word)  ; очищаю слово от разделителя
    (cdr
     (foldl (λ(char result)
              (define possible_delim (car result))  ; будущий разделитель
              (define new_word (cdr result))  ; новое слово
              (define str_char (string char))  ; строковое представление символа

              (if (equal? possible_delim delim)  ; проверяем, что разделитель накопился
                  (cons possible_delim (string-append new_word str_char))  ; если он накопился, то накапливаю слово
                  (cons (string-append possible_delim str_char) new_word)  ; иначе, накапливаю разделитель
                  )
              )
            (cons "" "")  ; будущий результат в формате — (будущий разделитель, будущее очищенное слово)
            (string->list word)  ; превращаем строку в список символов, чтобы можно было пройтись по ней foldl
            )
     )
    )

  (foldr (λ(char result)
           (define current_word (car result))  ; беру текущее слово
           (define str_char (string char))  ; строковое значение символа
           (define next_word (string-append str_char current_word))  ; вычисляю предполагаемое новое слово

           (cond
             [(not (string-contains? next_word delim)) (cons next_word (cdr result))]  ; заменяю старое слово новым, если новое слово не содержит разделитель полностью
             [(string-contains? next_word delim) (cons "" (cons (clean_word next_word) (cdr result)))]  ; добавляю новое слово без разделителя в результат и пустую строку, чтобы
                                                                                                        ; накапливать в неё новое слово
             )
           )
         '("")  ; результирующий список слов
         (string->list str)  ; превращаем строку в список символов, чтобы можно было пройтись по ней foldl
         )
  )