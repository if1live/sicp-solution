#!/usr/bin/env racket
#lang scheme

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (/ (- (square guess) x) x)) 0.001))

(define (sqrt x) (sqrt-iter 1.0 x))

(println (sqrt 1))
(println (sqrt 0.01))
(println (sqrt 0.0001))
