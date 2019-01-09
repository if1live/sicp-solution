#!/usr/bin/env racket
#lang scheme

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)