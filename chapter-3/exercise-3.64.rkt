#!/usr/bin/env racket
#lang racket

(require "../lib-common.rkt")
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

(define (stream-limit stream tolerance)
  (define (iter prev s)
    (if (< (abs (- (stream-car s) prev)) tolerance)
        (stream-car s)
        (iter (stream-car s) (stream-cdr s))))
  (iter (stream-car stream) (stream-cdr stream)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.1)
(sqrt 2 0.01)
(sqrt 2 0.001)
(sqrt 2 0.0001)
