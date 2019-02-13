#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")
(require "../lib-prime.rkt")

(define (merge-weighted s1 s2 weight)
  (define (pair-weight p) (weight (car p) (cadr p)))

  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((w1 (pair-weight (stream-car s1)))
               (w2 (pair-weight (stream-car s2))))
           (if (< w1 w2)
               (cons-stream (stream-car s1) (merge-weighted (stream-cdr s1) s2 weight))
               (cons-stream (stream-car s2) (merge-weighted s1 (stream-cdr s2) weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (weight-sum i j) (+ i j))
(define A (weighted-pairs integers integers weight-sum))
(stream-ref A 0)
(stream-ref A 1)
(stream-ref A 2)
(stream-ref A 3)
(stream-ref A 4)
(stream-ref A 5)
(stream-ref A 6)
(stream-ref A 7)
(stream-ref A 8)
(stream-ref A 9)


(println "-----------")

(define (weight-complex i j) (+ (* 2 i) (* 3 j) (* 5 i j)))

(define (not-divisible-2-3-5? x)
  (and (not (= (remainder x 2) 0))
       (not (= (remainder x 3) 0))
       (not (= (remainder x 5) 0))))
(define (check-fn p)
  (and (not-divisible-2-3-5? (car p))
       (not-divisible-2-3-5? (cadr p))))
(define B (stream-filter check-fn (weighted-pairs integers integers weight-complex)))

(stream-ref B 0)
(stream-ref B 1)
(stream-ref B 2)
(stream-ref B 3)
(stream-ref B 4)
(stream-ref B 5)
(stream-ref B 6)
(stream-ref B 7)
(stream-ref B 8)
(stream-ref B 9)
