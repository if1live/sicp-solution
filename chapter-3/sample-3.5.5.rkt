#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

; exercise 3.6
(define (make-rand-update a b m)
  (define (fn x)
      (remainder (+ (* a x) b) m))
    fn)

(define rand-update (make-rand-update 6053 7127 9973))

(define random-init 1)

;;;;;;;;;

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

(stream-ref pi 1)
(stream-ref pi 10)
(stream-ref pi 100)
(stream-ref pi 1000)
(stream-ref pi 10000)

