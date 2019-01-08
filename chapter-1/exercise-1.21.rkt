#!/usr/bin/env racket
#lang scheme


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
 (cond ((> (square test-divisor) n) n)
       ((divides? test-divisor n) test-divisor)
       (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square x) (* x x))

(println (prime? 7))
(println (prime? 21))
(println (prime? 23))

(println (smallest-divisor 199))
(println (smallest-divisor 1999))
(println (smallest-divisor 19999))




