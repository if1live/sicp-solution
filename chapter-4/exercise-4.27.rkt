#!/usr/bin/env racket
#lang racket

(require "sample-4.2.2.rkt")
(driver-loop)
(exit)

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))
(display count)
(display w)
(display count)
