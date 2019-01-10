#!/usr/bin/env racket
#lang scheme

(define (make-point x y) (cons x y))
(define (x-point x) (car x))
(define (y-point y) (cdr y))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (average a b) (/ (+ a b) 2))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment))
        (calc-midpoint (lambda (f a b) (average (f a) (f b)))))
       (make-point (calc-midpoint x-point start end)
                   (calc-midpoint y-point start end))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define p1 (make-point 1.0 2.0))
(define p2 (make-point 10.0 20.0))
(print-point (midpoint-segment (make-segment p1 p2)))
