#!/usr/bin/env racket
#lang racket

(require "../lib-common.rkt")
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

(define random-stream
  (stream-map (lambda (x) (exact->inexact (/ x 9973))) random-numbers))
  
  
(define (make-range-converter low high)
  (lambda (x)
    (let ((range (- high low)))
      (+ low (* x range)))))

(define (point-pairs x1 x2 y1 y2)
  (let ((convert-x (make-range-converter x1 x2))
        (convert-y (make-range-converter y1 y2)))
    (define (pairs s)
      (cons-stream (list (convert-x (stream-car s))
                         (convert-y (stream-car (stream-cdr s))))
                   (pairs (stream-cdr (stream-cdr s)))))
    (pairs random-stream)))

(define (integral-test f s)
  (stream-map (lambda (p) (f (car p) (cadr p))) s))

(define (in-circle? cx cy r x y)
  (let ((v1 (+ (square (- x cx)) (square (- y cy))))
        (v2 (square r)))
    (< v1 v2)))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (exact->inexact (/ passed (+ passed failed)))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (probability->area p x1 x2 y1 y2)
  (* p (- x2 x1) (- y2 y1)))

(define (estimate-integral f x1 x2 y1 y2)
  (stream-map
    (lambda (p) (probability->area p x1 x2 y1 y2))
    (monte-carlo (integral-test f (point-pairs x1 x2 y1 y2)) 0 0)))
                        
(define PI
  (estimate-integral (lambda (x y) (in-circle? 0 0 1 x y))
                     -1 1
                     -1 1))
(stream-ref PI 10000)

(define A
  (estimate-integral (lambda (x y) (in-circle? 5 7 3 x y))
                     2 8
                     4 10))
(stream-ref A 10000)

