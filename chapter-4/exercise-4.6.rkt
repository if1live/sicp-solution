#!/usr/bin/env racket
#lang racket


(define (eval exp env)
  (cond ((self-evaluating? exp) env)
        ((let? exp) (eval (let->combination exp) env))
        (else
         (error "unknown expression type -- EVAL" exp))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (make-begin seq) (cons 'begin seq))

(define (make-lambda vars body exps)
  (append (list (list 'lambda vars body)) exps))

(define (let? exp) (tagged-list? exp 'let))
(define (let-body exp) (cddr exp))
(define (let-clauses exp) (cadr exp))
(define (let-variable clause) (car clause))
(define (let-expression clause) (cadr clause))

(define (let->combination exp)
  (let ((clauses (let-clauses exp))
        (body (let-body exp)))
    (let ((vars (map let-variable clauses))
          (exps (map let-expression clauses)))
      (make-lambda vars (make-begin body) exps))))

(let->combination 
  '(let ((a 1) (b 2) (c 3)) 
     (println a)
     (+ a b c)))

