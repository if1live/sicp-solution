#!/usr/bin/env racket
#lang racket

(require "../lib-common.rkt")
(require "lib-stream.rkt")

;(define (pi-summands n)
;  (cons-stream (/ 1.0 n)
;               (stream-map - (pi-summands (+ n 2)))))
;(define pi-stream
;  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))



(println "----------")
(stream-ref ln2-stream 0)
(stream-ref ln2-stream 1)
(stream-ref ln2-stream 2)
(stream-ref ln2-stream 3)
(stream-ref ln2-stream 4)
(stream-ref ln2-stream 5)
(stream-ref ln2-stream 6)
(stream-ref ln2-stream 7)

(println "----------")
(define B (euler-transform ln2-stream))
(stream-ref B 0)
(stream-ref B 1)
(stream-ref B 2)
(stream-ref B 3)
(stream-ref B 4)
(stream-ref B 5)
(stream-ref B 6)
(stream-ref B 7)

(println "-------")
(define C (accelerated-sequence euler-transform ln2-stream))
(stream-ref C 0)
(stream-ref C 1)
(stream-ref C 2)
(stream-ref C 3)
(stream-ref C 4)
(stream-ref C 5)


