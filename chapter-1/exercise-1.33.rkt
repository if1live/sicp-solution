#!/usr/bin/env racket
#lang scheme

(define (accumulate combiner null-value term a next b)
  (define (iter a acc)
     (if (> a b) 
         acc
         (iter (next a) (combiner acc (term a)))))
  (iter a null-value))
      
(define (inc x) (+ x 1))
(define (identity x) x)

(define (filtered-accumulate combiner null-value term a next b predicate)
  (define (iter a acc)
    (cond ((> a b) acc)
          ((predicate a) (iter (next a) (combiner acc (term a))))
          (else (iter (next a) acc))))
  (iter a null-value))

(println (filtered-accumulate + 0 identity 1 inc 6 even?))


(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n) (= n (smallest-divisor n)))

(define (prime-sum a b)
  (filtered-accumulate + 0 square a inc b prime?))

(println (prime-sum 2 10))

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))


(define (product-gcd n)
  (define (gcd-is-one? x)
     (= (gcd n x) 1))
  (filtered-accumulate * 1 identity 2 inc (- n 1) gcd-is-one?))

(println (product-gcd 10))
