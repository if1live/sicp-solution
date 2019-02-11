#!/usr/bin/env racket
#lang racket

(require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))

(define (println x)
  (display x)
  (newline))

(define x 10)

(define s (make-serializer))

; 100 
; 1000 
; 10000
; 100000
; 1000000
(define (run)
  (set! x 10)
  (parallel-execute
    (lambda () (set! x (* x x)))
    (lambda () (set! x (* x x x)))))

(define (loop proc step)
  (define (iter counter)
    (if (> counter step)
        '()
        (begin (proc)
               (println x)
               (iter (+ counter 1)))))
  (iter 1))

(loop run 1000)
