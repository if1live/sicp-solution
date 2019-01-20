#!/usr/bin/env racket
#lang scheme

(define (make-vert x y) (list x y))
(define (xcor-vert v) (car v))
(define (ycor-vert v) (cadr v))

(define (add-vert a b)
  (make-vert (+ (xcor-vert a) (xcor-vert b))
             (+ (ycor-vert a) (ycor-vert b))))

(define (sub-vert a b)
  (make-vert (- (xcor-vert a) (xcor-vert b))
             (- (ycor-vert a) (ycor-vert b))))

(define (scale-vert s v)
  (make-vert (* s (xcor-vert v))
             (* s (ycor-vert v))))

(add-vert (make-vert 1 2) (make-vert 3 4))
(sub-vert (make-vert 1 2) (make-vert 3 4))
(scale-vert 2 (make-vert 1 2))
