#!/usr/bin/env racket
#lang scheme

(define (even? n)
  (= (remainder n 2) 0))

(define (square n) (* n n))

(define (fast-expt-iter b n)
  (define (iter b n step curr)
    (cond ((= step n) curr)
	  ((> (* step 2) n) (* curr (fast-expt-iter b (- n step)))) 
	  (else (iter b n (* step 2) (square curr)))))
  (iter b n 1 b))

(println (fast-expt-iter 2 1))
(println (fast-expt-iter 2 2))
(println (fast-expt-iter 2 4))
(println (fast-expt-iter 2 8))

(println (fast-expt-iter 2 9))
(println (fast-expt-iter 2 10))
(println (fast-expt-iter 2 11))

