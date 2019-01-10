#!/usr/bin/env racket
#lang scheme

(define (pow x n)
  (define (iter counter result)
    (if (> counter n) result (* x (iter (+ counter 1) result))))
  (iter 1 1))

(define (simple-log x base)
  (define (iter remain step)
    (if (= (remainder remain base) 0)
        (iter (/ remain base) (+ step 1))
        step))
  (iter x 0))

(define (cons x y) (* (pow 2 x) (pow 3 y)))
(define (car z) (simple-log z 2))
(define (cdr z) (simple-log z 3))

(car (cons 4 5))
(cdr (cons 4 5))
