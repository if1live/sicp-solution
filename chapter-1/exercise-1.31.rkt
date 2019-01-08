#!/usr/bin/env racket
#lang scheme

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product-rec term (next a) next b))))

(define (product-iter term a next b)
   (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (* result (term a)))))
   (iter a 1))

(define (inc x) (+ x 1))
(define (identity x) x)
(println (product-rec identity 1 inc 4))
(println (product-iter identity 1 inc 4))

(define (divide a b) (/ (- a (remainder a b)) b))

(define (calc-lower n)
  (+ (* (divide n 2) 2) 3))
(define (calc-upper n)
  (+ (* (divide (+ n 1) 2) 2) 2))

(define (calc-partial n)
  (/ (calc-upper n) (calc-lower n)))

(println (calc-partial 0))
(println (calc-partial 1))
(println (calc-partial 2))
(println (calc-partial 3))
(println (calc-partial 4))
(println (calc-partial 5))
(println (calc-partial 6))

(define (calc-pi-divide-4 step)
  (exact->inexact (product-iter calc-partial 0 inc step)))

(println (calc-pi-divide-4 1))
(println (calc-pi-divide-4 2))
(println (calc-pi-divide-4 3))
(println (calc-pi-divide-4 4))
(println (calc-pi-divide-4 5))
(println (calc-pi-divide-4 6))
(println (calc-pi-divide-4 7))
(println (calc-pi-divide-4 8))
(println (calc-pi-divide-4 9))
