#lang racket

(define (quicksort lst)
  (cond
    ((or (null? lst)
         (null? (cdr lst)))
     lst)
    (else
     (let* ((pivot (car lst))
            (rest (cdr lst))
            (comp (λ (com x) (com x pivot))))
       (append
        (quicksort
         (filter (curry comp <=) rest))
        (list pivot)
        (quicksort
         (filter (curry comp >) rest)))))))
