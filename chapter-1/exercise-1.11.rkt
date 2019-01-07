#!/usr/bin/env racket
#lang scheme

(define (f-rec n)
  (if (< n 3) 
    n
    (+ (f-rec (- n 1)) 
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))


(println (f-rec 1))
(println (f-rec 2))
(println (f-rec 3))
(println (f-rec 4))
(println (f-rec 5))


(define (f-iter n)
  (define (iter a b c counter)
    (if (= counter 2)
      a
      (iter (+ a (* 2 b) (* 3 c)) a b (- counter 1))))
  (if (< n 3) n (iter 2 1 0 n)))

(println (f-iter 1))
(println (f-iter 2))
(println (f-iter 3))
(println (f-iter 4))
(println (f-iter 5))
