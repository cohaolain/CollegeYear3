#lang racket


(define foo
  (λ (a b)
    (let ((bar (λ (a b n bar) (if (equal? 0 (+ (length a) (length b)))
                 (list)
                 (if (equal? 1 (modulo n 2))
                     (append (take a (min n (length a))) (bar (drop a (min n (length a))) b (+ n 1) bar))
                     (append (take b (min n (length b))) (bar a (drop b (min n (length b))) (+ n 1) bar))
                     ))))) (bar a b 1 bar))))