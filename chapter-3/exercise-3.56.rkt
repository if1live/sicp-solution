#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define (divisible? x y) (= (remainder x y) 0))


(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))));

(define (loop fn counter)
  (define (iter step)
    (if (>= step counter)
        '()
        (begin (fn step)
               (iter (+ step 1)))))
  (iter 0))


(loop (lambda (x) (println (stream-ref S x))) 20)
