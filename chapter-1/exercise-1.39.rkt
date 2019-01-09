#!/usr/bin/env racket
#lang scheme

(define (square x) (* x x))

(define (cont-frac n d k)
  (define (iter step result)
    (if (= step 0)
        result
        (iter (- step 1) (/ (n step) (+ (d step) result)))))
  (iter k 0))

(define (tan-cf x k)
  (let ((n (lambda (k) (if (= k 1) x (- (square x)))))
        (d (lambda (k) (- (* 2 k) 1.0))))
    (cont-frac n d k)))

; 45 deg = 0.785398 rad
(define angle 0.785398)
(tan-cf angle 1)
(tan-cf angle 2)
(tan-cf angle 3)
(tan-cf angle 4)
(tan-cf angle 5)
(tan-cf angle 6)
(tan-cf angle 7)
(tan-cf angle 8)
(tan-cf angle 9)
