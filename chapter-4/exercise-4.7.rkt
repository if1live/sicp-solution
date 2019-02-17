#!/usr/bin/env racket
#lang racket

; exercise 4.6
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


; exercise-4.7
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  (define (make-let var exp body)
    (cond ((let? body) (list 'let (list (list var exp)) body))
          ((= (length body) 1) (list 'let (list (list var exp)) (car body)))
          (else (append '(let) (list (list (list var exp))) body))))
  
  (define (make-inner-let vars exps body)
    (if (null? vars)
        body
        (make-let (car vars)
                  (car exps)
                  (make-inner-let (cdr vars) (cdr exps) body))))
  
  (let ((clauses (let-clauses exp))
        (body (let-body exp)))
    (let ((vars (map let-variable clauses))
          (exps (map let-expression clauses)))
      (make-inner-let vars exps body))))

(define A
  (let*->nested-lets
   '(let* ((x 3)
           (y (+ x 2))
           (z (+ x y 5)))
      (* x z))))
(println A)
(let->combination A)

; compare
(let->combination
 '(let ((x 3))
    (let ((y (+ x 2)))
      (let ((z (+ x y 5)))
        (* x z)))))

; multi line body
(let*->nested-lets
 '(let* ((x 3)
         (y (+ x 2))
         (z (+ x y 5)))
    (println x)
    (* x z)))

