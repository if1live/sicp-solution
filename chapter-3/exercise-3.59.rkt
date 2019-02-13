#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

; exercise-3.59.a
(define (integrate-series s)
  (define (make-coef a n)
    (/ a n))
  (stream-map make-coef s integers))

; 2 3 4 5 6 ...
(define A (add-streams integers ones))
(define B (cons-stream 99 (integrate-series A)))

(stream-ref B 0)
(stream-ref B 1)
(stream-ref B 2)
(stream-ref B 3)
(stream-ref B 4)

; exercise-3.59.b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(println "exp - integral")
(stream-ref exp-series 0)
(stream-ref exp-series 1)
(stream-ref exp-series 2)
(stream-ref exp-series 3)
(stream-ref exp-series 4)


(define cosine-series
  (cons-stream 1 (integrate-series (stream-map (lambda (x) (- x)) sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(println "cosine - integral")
(stream-ref cosine-series 0)
(stream-ref cosine-series 1)
(stream-ref cosine-series 2)
(stream-ref cosine-series 3)
(stream-ref cosine-series 4)

(println "sine - integral")
(stream-ref sine-series 0)
(stream-ref sine-series 1)
(stream-ref sine-series 2)
(stream-ref sine-series 3)
(stream-ref sine-series 4)
