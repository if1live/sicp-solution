#!/usr/bin/env racket
#lang racket

(require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))

(define (println x)
  (display x)
  (newline))

(define x 10)

(println "without serializer")
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))
(println x)

(println "with serializer")
(set! x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))
(println x)
