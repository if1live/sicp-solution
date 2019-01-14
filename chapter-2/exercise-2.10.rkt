#!/usr/bin/env racket
#lang scheme

(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (point-in-interval? x point)
  (and (<= (lower-bound x) point)
       (<= point (upper-bound x))))
       
(define (div-interval x y)
  (if (point-in-interval? y 0)
      (error "cannot divide by interval that has 0")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (lower-bound x)
  (min (car x) (cdr x)))

(define (upper-bound x)
  (max (car x) (cdr x)))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(div-interval (make-interval 1 2) (make-interval -10 0))
(div-interval (make-interval 1 2) (make-interval 0 10))
(div-interval (make-interval 1 2) (make-interval -10 10))
