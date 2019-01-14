#!/usr/bin/env racket
#lang scheme

(define (reverse items)
  (define (iter remain result)
    (if (null? remain)
        result
        (iter (cdr remain) (cons (car remain) result))))
  (iter items (list)))

(define (reverse-recur items)
  (if (null? items)
      '()
      (cons (reverse-recur (cdr items)) (car items))))

(reverse (list 1 4 9 16 25))
(reverse-recur (list 1 4 9 16 25))

