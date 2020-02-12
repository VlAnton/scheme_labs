#lang racket


(define (get_triple lst)
  (define idxs (build-list (length lst) values))
  (foldl (λ(i idx_i res_i)
           (foldl (λ(j idx_j res_j)
                    (cond
                      [(not (boolean? res_i)) res_i]
                      [(not (boolean? res_j)) res_j]
                      [(= idx_i idx_j) res_i]
                      [else
                       (foldl (λ(k idx_k res_k)
                                (cond
                                  [(not (boolean? res_k)) res_k]
                                  [(or (= idx_i idx_k) (= idx_j idx_k)) #f]
                                  [(= (+ i j) k) (list i j k)]
                                  [else #f]
                                  )
                                ) #f lst idxs
                                  )
                       ]
                      )
                    ) #f lst idxs
                      )
           ) #f lst idxs
             )
  )