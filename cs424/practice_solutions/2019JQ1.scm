#lang racket
(define scatter-gather
  (λ (i v)
     (map (λ (ti) (if (< ti (length v))
                      (last (take v (+ ti 1)))
                    #f))
          i)))