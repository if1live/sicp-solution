#!/usr/bin/env racket
#lang racket

(require "./lib-evaluator.rkt")

;; copy-and-paste

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(map (lambda (x) (+ x 1)) '(1 2 3))

