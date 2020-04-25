#lang racket

(define scatter-gather
  (λ (i v)
    (map (λ (ui) (if (< ui (length v))
                     (last (take v (+ ui 1)))
                     #f)
           )
         i)))

