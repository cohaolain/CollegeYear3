#lang racket

(define unflat
  (λ (l) (apply append (map (λ (e) (if (list? e) (unflat e) (list e))) l))))

(define fil
  (λ (p? l) (if (null? l) (list) (if (p? (car l)) (append (list (car l)) (fil p? (cdr l))) (fil p? (cdr l))))))

(define foo
  (λ (p? l)
    (fil p? (unflat l))))