#!/usr/bin/env racket
#lang scheme

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))

(define (square x) (* x x))

(define (square-list-a items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-a (cdr items)))))

(define (square-list-b items)
  (map square items))

(square-list-a (list 1 2 3 4))
(square-list-b (list 1 2 3 4))

