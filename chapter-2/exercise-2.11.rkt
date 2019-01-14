#!/usr/bin/env racket
#lang scheme

(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (neg? x) (negative? x))
(define (pos? x) (positive? x))

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond ((and (neg? x1) (neg? x2) (neg? y1) (neg? y2)) (make-interval (* x2 y2) (* x1 y1)))
          ((and (neg? x1) (neg? x2) (neg? y1) (pos? y2)) (make-interval (* x1 y2) (* x1 y1)))
          ((and (neg? x1) (neg? x2) (pos? y1) (pos? y2)) (make-interval (* x1 y2) (* x2 y1)))
          ((and (neg? x1) (pos? x2) (neg? y1) (neg? y2)) (make-interval (* x2 y1) (* x1 y1)))
          ((and (neg? x1) (pos? x2) (neg? y1) (pos? y2)) 
            (make-interval (min (* x1 y2) (* x2 y1)) (max (* x1 y1) (* x2 y2))))
          ((and (neg? x1) (pos? x2) (pos? y1) (pos? y2)) (make-interval (* x1 y2) (* x2 y2)))
          ((and (pos? x1) (pos? x2) (neg? y1) (neg? y2)) (make-interval (* x2 y1) (* x1 y2)))
          ((and (pos? x1) (pos? x2) (neg? y1) (pos? y2)) (make-interval (* x2 y1) (* x2 y2)))
          ((and (pos? x1) (pos? x2) (pos? y1) (pos? y2)) (make-interval (* x1 y1) (* x2 y2)))
          (else (error "unknown case")))))

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

(mul-interval (make-interval -1 3) (make-interval -10 20))
(mul-interval (make-interval -3 1) (make-interval -20 10))

(mul-interval (make-interval -2 -1) (make-interval -20 -10))
(mul-interval (make-interval -2 -1) (make-interval -10 10))
(mul-interval (make-interval -2 -1) (make-interval 10 20))

(mul-interval (make-interval -1 1) (make-interval -20 -10))
(mul-interval (make-interval -1 1) (make-interval -10 10))
(mul-interval (make-interval -1 1) (make-interval 10 20))

(mul-interval (make-interval 1 2) (make-interval -20 -10))
(mul-interval (make-interval 1 2) (make-interval -10 10))
(mul-interval (make-interval 1 2) (make-interval 10 20))
