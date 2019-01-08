#!/usr/bin/env racket
#lang scheme

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a acc)
     (if (> a b) 
         acc
         (iter (next a) (combiner acc (term a)))))
  (iter a null-value))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate-rec combiner (combiner null-value (term a)) term (next a) next b)))
      
(define (inc x) (+ x 1))
(define (identity x) x)

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (sum-rec term a next b)
  (accumulate-rec + 0 term a next b))

(define (product-rec term a next b)
  (accumulate-rec * 1 term a next b))


(println (sum-iter identity 1 inc 4))
(println (product-iter identity 1 inc 4))
(println (sum-rec identity 1 inc 4))
(println (product-rec identity 1 inc 4))
