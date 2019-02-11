#!/usr/bin/env racket
#lang racket

(require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))

(define (println x)
  (display x)
  (newline))

(define x 10)

(define s (make-serializer))

; 100
(define (run)
  (set! x 10)
  (parallel-execute
    (lambda () (set! x ((s (lambda () (* x x)))))
    (s (lambda () (set! x (+ x 1)))))))

(define (loop proc step)
  (define (iter counter)
    (if (> counter step)
        '()
        (begin (proc)
               (println x)
               (iter (+ counter 1)))))
  (iter 1))

(loop run 100)
