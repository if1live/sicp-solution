#!/usr/bin/env racket
#lang scheme

(define (cont-frac n d k)
  (define (cont-frac-rec n d k step)
    (if (= step k) 
        (/ (n k) (d k))
        (/ (n step) (+ (d step) (cont-frac-rec n d k (+ step 1))))))
  (cont-frac-rec n d k 1))

(define (cont-frac-iter n d k)
  (define (iter step result)
    (if (= step 0)
        result
        (iter (- step 1) (/ (n step) (+ (d step) result)))))
  (iter k 0))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 2)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 3)

(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 1)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 2)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 3)


(define (close-enough? guess x)
(< (abs (- guess x)) 0.00005))

(define (find-close n d)
  (define (iter step)
    (let ((prev-value (cont-frac-iter n d step))
          (next-value (cont-frac-iter n d (+ step 1))))
          (if (close-enough? prev-value next-value)
              step
              (iter (+ step 1)))))
  (iter 0))

(find-close (lambda (i) 1.0) (lambda (i) 1.0))
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 12)