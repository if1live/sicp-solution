#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

; exercise-3.59.a
(define (integrate-series s)
  (define (make-coef a n)
    (/ a n))
  (stream-map make-coef s integers))

; exercise-3.59.b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map (lambda (x) (- x)) sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))
                            (add-streams (stream-map (lambda (x) (* x (stream-car s2))) (stream-cdr s1))
                                         (stream-map (lambda (x) (* x (stream-car s1))) (stream-cdr s2))))))


(define A (mul-series cosine-series cosine-series))
(define B (mul-series sine-series sine-series))
(define C (add-streams A B))
;(define C (mul-series integers integers))

(stream-ref C 0)
(stream-ref C 1)
(stream-ref C 2)
(stream-ref C 3)
(stream-ref C 4)
