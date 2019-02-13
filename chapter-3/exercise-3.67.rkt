#!/usr/bin/env racket
#lang racket

(require "../lib-prime.rkt")
(require "lib-stream.rkt")

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers integers))

(stream-ref int-pairs 0)
(stream-ref int-pairs 1)
(stream-ref int-pairs 2)
(stream-ref int-pairs 3)
(stream-ref int-pairs 4)
(stream-ref int-pairs 5)
(stream-ref int-pairs 6)
(stream-ref int-pairs 7)
(stream-ref int-pairs 8)
(stream-ref int-pairs 9)
(stream-ref int-pairs 10)
