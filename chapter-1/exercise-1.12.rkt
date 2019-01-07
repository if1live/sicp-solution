#!/usr/bin/env racket
#lang scheme

(define (pascal-triangle-elem n k)
  (cond ((= n 1) 1)
	((= k 1) 1)
	((= k n) 1)
	(else (+ (pascal-triangle-elem (- n 1) (- k 1))
		 (pascal-triangle-elem (- n 1) k)))))

(println (pascal-triangle-elem 5 1))
(println (pascal-triangle-elem 5 2))
(println (pascal-triangle-elem 5 3))
(println (pascal-triangle-elem 5 4))
(println (pascal-triangle-elem 5 5))

