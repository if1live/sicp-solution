#!/usr/bin/env racket
#lang scheme

(define (gcd a b)
   (if (= b 0) a (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d)))
        (abs-n (abs n))
        (abs-d (abs d)))
    (if (positive? (* n d))
        (cons (/ abs-n g) (/ abs-d g))
        (cons (* -1 (/ abs-n g)) (/ abs-d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(print-rat (make-rat 6 4))
(print-rat (make-rat -6 4))
(print-rat (make-rat 6 -4))
(print-rat (make-rat -6 -4))

