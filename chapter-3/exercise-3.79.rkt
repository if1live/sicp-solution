#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand  dt)
                                int))))
  int)

(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define (f dy y)
  (let ((a 1)
        (b 2))
    (+ (* a dy) (* b y))))

(stream-ref (solve-2nd f 1 1.1 0.001) 1000)
