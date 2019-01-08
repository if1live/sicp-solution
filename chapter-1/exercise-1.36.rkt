#!/usr/bin/env racket
#lang scheme

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value) (search f neg-point midpoint))
                ((negative? test-value) (search f midpoint pos-point))
                (else midpoint))))))


(define (average a b) (/ (+ a b) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (positive? a-value) (negative? b-value)) (search f b a))
          (else error "Values are not of opposite sign" a b))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (newline)
    (display "guess - ")
    (display v1)
    (display " / ")
    (display "error - ")
    (display (- v1 v2))
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))


(define (calc-my-log)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)) 
(calc-my-log)
