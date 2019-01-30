#!/usr/bin/env racket
#lang racket

(define (make-accumulator x)
  (define (add val)
    (set! x (+ x val))
    x)
  add)

(define A (make-accumulator 5))
(A 10)
(A 10)