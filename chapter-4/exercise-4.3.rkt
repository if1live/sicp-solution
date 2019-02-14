#!/usr/bin/env racket
#lang racket

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (eval exp env)
  (cond ((self-evaluating? exp) env)
        ((variable? exp) (lookup-variable-value exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else ((get 'eval (operator exp)) exp env))))

(put 'eval 'quote text-of-quotation)
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (exp env) 
                     (make-procedure (lambda-paramters exp)
                                     (lambda-body exp)
                                     env)))
(put 'eval 'begin (lambda (exp env)
                    (eval-sequence (begin-actions exp) env)))

