#lang racket
;(define tear
;  (λ (p? xs)
;    (list (filter p? xs)
;          (filter (λ (x) (not (p? x))) xs))))

(define bfilter
  (λ (p? l)
     (cond ((null? l) '())
           ((p? (car l)) (cons (car l) (bfilter p? (cdr l))))
           (else (bfilter p? (cdr l))))))

(define bad-tear
  (λ (p? l)
     (list (cond ((null? l) '())
                 ((p? (car l)) (cons (car l) (car (bad-tear p? (cdr l)))))
                 (else (car (bad-tear p? (cdr l)))))

           (cond ((null? l) '())
                 (((λ (x) (not (p? x))) (car l)) (cons (car l) (cadr (bad-tear p? (cdr l)))))
                 (else (cadr (bad-tear p? (cdr l))))))))
           