#!/usr/bin/env racket
#lang racket

(require "sample-4.2.2.rkt")
(driver-loop)
(exit)

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))
(car '(a b c))
(car (cdr '(a b c)))