#lang racket

(define map-skip
  (λ (f l) 
    (cond ((> (length l) 1)
           (append (list (f (car l)) (cadr l)) (map-skip f (cddr l))))
          ((equal? (length l) 1) (list (f (car l))))
          (else (list)))))