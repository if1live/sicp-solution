#!/usr/bin/env racket
#lang scheme

(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (neg? x) (negative? x))
(define (pos? x) (positive? x))

;(define (mul-interval x y)
;  (let ((x1 (lower-bound x))
;        (x2 (upper-bound x))
;        (y1 (lower-bound y))
;        (y2 (upper-bound y)))
;    (cond ((and (neg? x1) (neg? x2) (neg? y1) (neg? y2)) (make-interval (* x2 y2) (* x1 y1)))
;          ((and (neg? x1) (neg? x2) (neg? y1) (pos? y2)) (make-interval (* x1 y2) (* x1 y1)))
;          ((and (neg? x1) (neg? x2) (pos? y1) (pos? y2)) (make-interval (* x1 y2) (* x2 y1)))
;          ((and (neg? x1) (pos? x2) (neg? y1) (neg? y2)) (make-interval (* x2 y1) (* x1 y1)))
;          ((and (neg? x1) (pos? x2) (neg? y1) (pos? y2)) 
;            (make-interval (min (* x1 y2) (* x2 y1)) (max (* x1 y1) (* x2 y2))))
;          ((and (neg? x1) (pos? x2) (pos? y1) (pos? y2)) (make-interval (* x1 y2) (* x2 y2)))
;          ((and (pos? x1) (pos? x2) (neg? y1) (neg? y2)) (make-interval (* x2 y1) (* x1 y2)))
;          ((and (pos? x1) (pos? x2) (neg? y1) (pos? y2)) (make-interval (* x2 y1) (* x2 y2)))
;          ((and (pos? x1) (pos? x2) (pos? y1) (pos? y2)) (make-interval (* x1 y1) (* x2 y2)))
;          (else (error "unknown case")))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

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

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (make-center-percent c p)
  (let ((ratio (* p 0.01)))
    (make-center-width c (* c ratio))))

(define (percent-ratio i)
  (/ (width i) (center i)))

(define (percent i)
  (* (percent-ratio i) 100))

(define (mul2-interval x y)
  (let ((c (* (center x) (center y)))
        (px (percent-ratio x))
        (py (percent-ratio y)))
    (make-center-percent (* c (+ 1 (* px py))) (* (+ px py) 100))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))


(define A (make-center-width 100 0.1))
(define B (make-center-width 100 0.2))
(define C (make-center-width 100 0))

;(par1 A B)
;(par2 A B)

(par1 A A)
(par2 A A)

(par1 C C)
(par2 C C)
