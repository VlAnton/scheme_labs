#lang racket


(define (eof? line) (equal? line eof))

(define (member? element lst)
  (list? (member element lst)))

(define (not_member? element lst)
  (not (member? element lst)))

; 1

(define (count-isolated filename)
  (define in (open-input-file filename))
  
  (define (next)
    (define row (read-line in))
    (if (eof? row)
        row
        (string-split row " "))
    )

  (define (iter row node isolated)
    (define non-isolated
      (if (eof? row)
          '()
          (foldl
           (λ(flag inner_node res)
             (if (and (not (equal? flag "0")) (not (equal? inner_node node)))
                 (cons inner_node res)
                 res))
           '() row (range (length row)))
          )
      )
    (define (inner node non-isolated isolated)
      (cond
        [(empty? non-isolated) (iter (next) (+ node 1) isolated)]
        [else (inner node (cdr non-isolated) (remove (car non-isolated) isolated))]
        )
      )

    (cond
      [(eof? row) (begin
                    (close-input-port in)
                    (length isolated))]
      [(empty? non-isolated) (iter (next) (+ node 1) isolated)]
      [else (inner node (cons node non-isolated) isolated)]
      )
    )
  (define first-row (next))
  (define isolated-init (range (length first-row)))
  (iter first-row 0 isolated-init)
  )


; 2 — работает вроде

(define (multiple? node nodes)
  (and (not_member? node nodes) (boolean? (check-duplicates nodes)))
  )

(define (complete? filename)
  (define in (open-input-file filename))
  (define (next)
    (define row (read-line in))
    (if (eof? row)
        row
        (string-split row " "))
    )
  (define (iter-table node)
    (define row (next))
    (define amount_node
      (if (eof? row)
          #f
          (string->number (car row)))
      )
    (cond
      [(eof? row) (begin
                    (close-input-port in)
                    #t)]
      [(not (and (equal? (- amount 1) amount_node) (multiple? node (cdr row))))
       (begin
         (close-input-port in)
         #f)]
      [else (iter-table (+ node 1))]
      )
    )
  (define amount (string->number (car (next))))
  (iter-table 0)
  )


; 3

(define (to_str pair)
  (string-append (number->string (car pair)) " " (number->string (cdr pair))))

(define (used-edge? edge edges)
  (or (member? edge edges)
      (member? (cons (cdr edge) (car edge)) edges)))

(define (loop? edge)
  (equal? (car edge) (cdr edge)))

(define (write-edges-from-table filename)
  (define in (open-input-file filename))
  (define out (open-output-file #:exists 'replace "output.txt"))

  (define (next)
    (define row (read-line in))
    (if (eof? row)
        row
        (cdr (map string->number (string-split row " "))))
    )

  (define (iter-table node edges)
    (define nodes (next))

    (define (write-edges)
      (define (iter-nodes i_nodes used_edges)
        (cond
          [(empty? i_nodes) used_edges]
          [else
           (let ([edge (cons node (car i_nodes))])
             (if (or (loop? edge) (used-edge? edge used_edges))
                 (iter-nodes (cdr i_nodes) used_edges)
                 (begin
                   (fprintf out "~a\n" (to_str edge))
                   (iter-nodes (cdr i_nodes) (cons edge used_edges)))
                 )
             )
           ]
          )
        )
      (iter-table (+ 1 node) (iter-nodes nodes edges))
      )

    (cond
      [(eof? nodes) (begin
                    (close-input-port in)
                    (close-output-port out))]
      [(empty? nodes) (iter-table (+ node 1) edges)]
      [else (write-edges)]
      )
    )
  (define amount (next))
  (iter-table 0 '())
  )


; 4


(define (write-edges-from-matrix filename)
  (define in (open-input-file filename))
  (define out (open-output-file #:exists 'replace "output.txt"))

  (define (next)
    (define row (read-line in))
    (if (eof? row)
        row
        (map string->number (string-split row " ")))
    )

  (define (iter-matr node edges)
    (define row (next))

    (define (write-edges)
      (define (iter-nodes i_node edge_flags used_edges)
        (cond
          [(empty? edge_flags) used_edges]
          [else
           (let ([edge (cons node i_node)])
             (if (or (loop? edge) (equal? (car edge_flags) 0) (used-edge? edge used_edges))
                 (iter-nodes (+ i_node 1) (cdr edge_flags) used_edges)
                 (begin
                   (fprintf out "~a\n" (to_str edge))
                   (iter-nodes (+ i_node 1) (cdr edge_flags) (cons edge used_edges)))
                 )
             )
           ]
          )
        )
      (iter-matr (+ 1 node) (iter-nodes 0 row edges))
      )

    (cond
      [(eof? row) (begin
                      (close-input-port in)
                      (close-output-port out))]
      [else (write-edges)]
      )
    )
  (iter-matr 0 '())
  )


; 5


(define (table->matrix filename)
  (define in (open-input-file filename))
  (define out (open-output-file #:exists 'replace "output.txt"))

  (define (next)
    (define row (read-line in))
    (if (eof? row)
        row
        (map string->number (string-split row " ")))
    )

  (define nodes-amount (car (next)))

  (define (iter node)
    (define row (next))

    (define (write-row inner_node inner_row)
      (define (write-zero)
        (begin
           (fprintf out "~a " "0")
           (write-row (+ inner_node 1) inner_row)))
      (define (write-one)
        (begin
          (fprintf out "~a " "1")
          (write-row (+ inner_node 1) (remove inner_node inner_row))))
        
      (cond
        [(= inner_node nodes-amount) (begin
                                       (fprintf out "~a" "\n")
                                       (iter (+ node 1)))]
        [(empty? inner_row) (write-zero)]
        [(member? inner_node inner_row) (write-one)]
        [else (write-zero)]
        )
      )

    (cond
      [(eof? row) (begin
                    (close-input-port in)
                    (close-output-port out))]
      [(empty? (cdr row)) (begin
                            (for-each (λ(node) (fprintf out "~a " "0")) (range nodes-amount))
                            (fprintf out "~a" "\n")
                            (iter (+ node 1)))]
      [else (write-row 0 (cdr row))]
      )
    )
  (iter 0)
  )