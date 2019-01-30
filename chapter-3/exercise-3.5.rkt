#!/usr/bin/env racket
#lang racket

(define (square x) (* x x))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (test) (integral-test p x1 x2 y1 y2))
  (* (monte-carlo trials test) (- x2 x1) (- y2 y1)))

(define (integral-test p x1 x2 y1 y2)
  (let ((x (random-in-range x1 x2))
        (y (random-in-range y1 y2)))
    (p x y)))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; racket 기준으로 (random N)하면 정수가 나온다
; 그래서 변경
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (in-circle? cx cy r x y)
  (< (+ (square (- x cx)) (square (- y cy)))
     (square r)))

(define PI
  (estimate-integral (lambda (x y) (in-circle? 0 0 1 x y))
                     -1 1
                     -1 1
                     100000))
(exact->inexact PI)

(define A
  (estimate-integral (lambda (x y) (in-circle? 5 7 3 x y))
                     2 8
                     4 10
                     100000))
(exact->inexact A)