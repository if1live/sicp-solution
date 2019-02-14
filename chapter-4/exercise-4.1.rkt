#!/usr/bin/env racket
#lang racket

; fake method
(define (no-operands? x) (null? x))
(define (eval exp env) 
  (display exp)
  (display "  ")
  exp)
(define (first-operand exps) (car exps))
(define (rest-operands exps) (cdr exps))


(define (list-of-values-ltor exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
        (cons first-value 
              (list-of-values-ltor (rest-operands exps) env)))))


(define (list-of-values-rtol exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values-rtol (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
             rest-values))))

(list-of-values-ltor '(1 2 3 4) '())
(println "----------")
(list-of-values-rtol '(1 2 3 4) '())
