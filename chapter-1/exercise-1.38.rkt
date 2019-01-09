#!/usr/bin/env racket
#lang scheme

(define (cont-frac n d k)
  (define (iter step result)
    (if (= step 0)
        result
        (iter (- step 1) (/ (n step) (+ (d step) result)))))
  (iter k 0))

(define (divide a b)
  (/ (- a (remainder a b)) b))

(define (d k)
  (cond ((= (remainder k 3) 2) (+ 2 (* 2 (divide k 3))))
        (else 1)))

(define (n k) 1.0)

(define (find-e k)
  (+ 2 (cont-frac n d k)))

(find-e 1)
(find-e 2)
(find-e 3)
(find-e 4)
(find-e 5)
(find-e 6)
(find-e 7)
(find-e 8)
(find-e 9)
