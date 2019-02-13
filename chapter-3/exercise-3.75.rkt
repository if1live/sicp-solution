#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

; exercise-3.74
(define (list->stream data)
  (if (null? data)
      the-empty-stream
      (cons-stream (car data) (list->stream (cdr data)))))

(define sense-data
  (list->stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define (sign-change-detector next prev)
  (cond ((and (positive? next) (negative? prev)) 1)
        ((and (negative? next) (positive? prev)) -1)
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream) avpt))))

(define zero-crossings (make-zero-crossings sense-data 0))

(display-stream zero-crossings)

; ??????????