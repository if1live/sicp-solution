#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define A (expand 1 7 10))
(stream-ref A 0)
(stream-ref A 1)
(stream-ref A 2)
(stream-ref A 3)
(stream-ref A 4)
(stream-ref A 5)
(stream-ref A 6)
(stream-ref A 7)

(println "----")
(define B (expand 3 8 10))
(stream-ref B 0)
(stream-ref B 1)
(stream-ref B 2)
(stream-ref B 3)
(stream-ref B 4)
(stream-ref B 5)
(stream-ref B 6)
(stream-ref B 7)
