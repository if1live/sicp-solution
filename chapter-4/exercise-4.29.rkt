#!/usr/bin/env racket
#lang racket

(require "sample-4.2.2.rkt")
(driver-loop)
(exit)

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)


(define (square x) (* x x))

; 100
(square (id 10))

; simple -> 1
; memo -> 2
; simple의 경우 square가 2번 전개되니까
(display count)