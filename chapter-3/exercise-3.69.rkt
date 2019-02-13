#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")
(require "../lib-prime.rkt")

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (interleave (stream-map (lambda (u0) (list (stream-car s) (stream-car t) u0))
                            (stream-cdr u))
                (stream-map (lambda (pair) (append (list (stream-car s)) pair))
                            (pairs (stream-cdr t) (stream-cdr u))))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


(define int-triples (triples integers integers integers))

(stream-ref int-triples 0)
(stream-ref int-triples 1)
(stream-ref int-triples 2)
(stream-ref int-triples 3)
(stream-ref int-triples 4)
(stream-ref int-triples 5)
(stream-ref int-triples 6)
(stream-ref int-triples 7)
(stream-ref int-triples 8)
(stream-ref int-triples 9)
