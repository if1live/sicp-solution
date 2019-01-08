#!/usr/bin/env racket
#lang scheme

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (define (iter a b step acc)
    (cond ((= b 0) acc) 
          ((= step b) (double acc))
          ((> step b) (+ acc (fast-mul a b)))
	  (else (iter a (- b step) (double step) (double acc)))))
  (iter a (- b 1) 1 a))

(println (fast-mul 5 1))
(println (fast-mul 5 2))
(println (fast-mul 5 3))
(println (fast-mul 5 4))
(println (fast-mul 5 5))
(println (fast-mul 5 6))
(println (fast-mul 5 7))
(println (fast-mul 5 8))
   
