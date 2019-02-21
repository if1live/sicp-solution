#!/usr/bin/env racket
#lang racket

(require compatibility/mlist)


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (make-begin seq) (cons 'begin seq))


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda vars body exps)
  (append (list (list 'lambda vars body)) exps))

(define (body-defines exps) (filter definition? exps))
(define (body-exps exps) (filter (lambda (x) (not (definition? x))) exps))
        
(define (scan-out-defines exp)
  (let ((definitions (body-defines exp))
        (inner-body (body-exps exp)))
    (if (= (length definitions) 0)
        exp
        (let ((let-vars (map (lambda (exp)
                               (list (definition-variable exp) '*unassigned*))
                             definitions))
              (let-sets (map (lambda (exp)
                               (list 'set! (definition-variable exp) (definition-value exp)))
                             definitions)))
          (list (append (list 'let let-vars)
                        (append let-sets inner-body)))))))

(scan-out-defines
 (lambda-body
  '(lambda (a b)
     (define u (+ 1 2))
     (define v (+ 3 4))
     (display u)
     (display v))))


(scan-out-defines
 (lambda-body
  '(lambda (a b)
     (display a)
     (display b))))

