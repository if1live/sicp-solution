#!/usr/bin/env racket
#lang scheme

(define (reverse items)
  (define (iter remains results)
    (if (null? remains) 
        results
        (iter (cdr remains) (cons (car remains) results))))
  (iter items (list)))

(define (same-parity x . rest)
  (define (check-then-handle f val results)
    (if (f val) (cons val results) results))

  (define (check-iter f items results)
    (if (null? items)
        results
        (check-iter f (cdr items) (check-then-handle f (car items) results))))

  (define (select-check-fn x)
    (if (= (remainder x 2) 0) even? odd?))

  (let ((f (select-check-fn x)))
       (reverse (check-iter f (cons x rest) (list)))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
