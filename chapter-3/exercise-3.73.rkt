#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)


; r: R = 저항
; c: C = 축전기
(define (RC r c dt)
  (define (voltage i-stream v0)
    (add-streams (scale-stream i-stream r)
                 (integral (scale-stream i-stream (/ 1 c)) v0 dt)))
  voltage)
  

(define RC1 (RC 5 1 0.5))

(define i integers)
(define v0 0.5)
(define v (RC1 i 0.5))
(stream-ref v 0)
(stream-ref v 1)
(stream-ref v 2)
(stream-ref v 3)
(stream-ref v 4)
