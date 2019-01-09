#!/usr/bin/env racket
#lang scheme

(define (smooth f dx)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

((smooth (lambda (x) (* x x)) 0.01) 1)

; (0.99**2 + 1**2 + 1.01**2)/3

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

(define (n-smooth f dx n)
  (repeated (smooth f dx) n))

((n-smooth (lambda (x) (* x x)) 0.01 5) 1)
