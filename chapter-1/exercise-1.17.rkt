#!/usr/bin/env racket
#lang scheme


(define (mul a b)
  (if (= b 0)
    0
    (+ a (mul a (- b 1)))))


(define (double x) (* x 2))
(define (halve x) (/ x 2))

(println (mul 4 500))


(define (fast-mul a b)
  (cond ((= b 0) 0)
	((= 0 (remainder b 2)) (fast-mul (double a) (halve b)))
	(else (+ a (fast-mul a (- b 1))))))

(println (fast-mul 4 499))
(println (fast-mul 4 500))
(println (fast-mul 4 501))
   
