#!/usr/bin/env racket
#lang scheme

(define (square x) (* x x))

(define (sum-two-square a b) (+ (square a) (square b)))

(define (f a b c)
  (cond ((and (< a b) (< a c)) (sum-two-square b c))
	((and (< b a) (< b c)) (sum-two-square a c))
	(else (sum-two-square a b))))

(println (f 1 2 3))
(println (f 4 1 3))
