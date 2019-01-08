#!/usr/bin/env racket
#lang scheme

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (cube x) (* x x x))

(println (integral cube 0 1 0.01))
(println (integral cube 0 1 0.001))


(define (integral-simpson-rule f a b n)
  (define (calc-h) (/ (- b a) n))
  (define (calc-coef step)
    (cond ((= step 0) 1)
          ((= step n) 1)
          ((= (remainder step 2) 1) 4)
          (else 2)))
  (define (calc-x step) (+ a (* (calc-h) step)))
  (define (new-f step)
    (* (f (calc-x step)) (calc-coef step)))
  (define (inc x) (+ x 1))
    
  (* (sum new-f 0 inc n) (/ (calc-h) 3.0)))

(integral-simpson-rule cube 0 1 100)
(integral-simpson-rule cube 0 1 1000)

