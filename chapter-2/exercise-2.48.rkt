#!/usr/bin/env racket
#lang racket

(define (make-vert x y) (list x y))
(define (xcor-vert v) (car v))
(define (ycor-vert v) (cadr v))

(define (make-segment start-vert end-vert)
  (list start-vert end-vert))

(define (start-segment s) (car s))
(define (end-segment s) (cadr s))

(define A (make-segment (make-vert 1 2) (make-vert 3 4)))
(start-segment A)
(end-segment A)