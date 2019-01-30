#!/usr/bin/env racket
#lang racket

(define (make-rand-update a b m)
  (define (fn x)
    (remainder (+ (* a x) b) m))
  fn)

(define rand-update (make-rand-update 6053 7127 9973))

(define random-init 1)

(define rand
  (let ((x random-init))
    (define (generate)
      (set! x (rand-update x))
      x)

    (define (reset init-val)
      (set! x init-val)
      #t)
      
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else (error "unknown request -- rand" m))))
    dispatch))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

((rand 'reset) 1)
(rand 'generate)
(rand 'generate)