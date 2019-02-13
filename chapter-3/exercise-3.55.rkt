#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

(define integer-partial-sums (partial-sums integers))

; 1
(stream-ref integer-partial-sums 0)

; 1+2=3
(stream-ref integer-partial-sums 1)

; 1+2+3=6
(stream-ref integer-partial-sums 2)

; 1+2+3+4=10
(stream-ref integer-partial-sums 3)
