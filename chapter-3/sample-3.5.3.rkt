#!/usr/bin/env racket
#lang racket

(require "../lib-common.rkt")
(require "../lib-prime.rkt")
(require "lib-stream.rkt")

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)

(println "----------")
(define A (sqrt-stream 2))
(stream-ref A 0)
(stream-ref A 1)
(stream-ref A 2)
(stream-ref A 3)
(stream-ref A 4)



(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(println "----------")
(stream-ref pi-stream 0)
(stream-ref pi-stream 1)
(stream-ref pi-stream 2)
(stream-ref pi-stream 3)
(stream-ref pi-stream 4)
(stream-ref pi-stream 5)
(stream-ref pi-stream 6)
(stream-ref pi-stream 7)



(println "----------")
(define B (euler-transform pi-stream))
(stream-ref B 0)
(stream-ref B 1)
(stream-ref B 2)
(stream-ref B 3)
(stream-ref B 4)
(stream-ref B 5)
(stream-ref B 6)
(stream-ref B 7)


(println "-------")
(define C (accelerated-sequence euler-transform pi-stream))
(stream-ref C 0)
(stream-ref C 1)
(stream-ref C 2)
(stream-ref C 3)
(stream-ref C 4)
(stream-ref C 5)

(println "-------")

(define int-pairs (pairs integers integers))

(define D (stream-filter (lambda (pair) (prime? (+ (car pair) (cadr pair)))) int-pairs))
(stream-ref D 0)
(stream-ref D 1)
(stream-ref D 2)
(stream-ref D 3)
(stream-ref D 4)
(stream-ref D 5)

(println "-------")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)
