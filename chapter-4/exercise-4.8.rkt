#!/usr/bin/env racket
#lang racket

; exercise-4.6
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
;(define (let-body exp) (cddr exp))
;(define (let-clauses exp) (cadr exp))
;(define (let-variable clause) (car clause))
;(define (let-expression clause) (cadr clause))
(define (let-var exp) (cadr exp))
(define (let-bindings exp) (caddr exp))
(define (let-body exp) (cdddr exp))
(define (bindings-var b) (car b))
(define (bindings-exp b) (cadr b))

;(define (let->combination exp)
;  (let ((clauses (let-clauses exp))
;        (body (let-body exp)))
;    (let ((vars (map let-variable clauses))
;          (exps (map let-expression clauses)))
;      (make-lambda vars (make-begin body) exps))))

(define (let->combinations exp)
  (let ((var (let-var exp))
        (bindings (let-bindings exp))
        (body (let-body exp)))
    (let ((vars (map bindings-var bindings))
          (exps (map bindings-exp bindings)))
      (list
       'begin
       (append '(define)
             (list (append (list var) vars))
             body)
       (append (list var) exps)))))

(define A
  '(let fib-iter ((a 1)
                  (b 0)
                  (count n))
     (if (= count 0)
         b
         (fib-iter (+ a b) a (- count 1)))))
(let->combinations A)

;(define n 4)
;(begin
;  (define (fib-iter a b count)
;    (if (= count 0)
;        b
;        (fib-iter (+ a b) a (- count 1))))
;  (fib-iter 1 0 n))
