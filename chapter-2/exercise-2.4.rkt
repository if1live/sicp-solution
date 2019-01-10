#!/usr/bin/env racket
#lang scheme

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(define x 10)
(define y 20)
(car (cons x y))
(cdr (cons x y))
