#!/usr/bin/env racket
#lang racket

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))


(define (do? exp) (tagged-list? exp 'do))
(define (do-bindings exp) (cadr exp))
(define (do-clause exp) (caddr exp))
(define (do-body exp) (cdddr exp))

(define (binding-variable binding) (car binding))
(define (binding-init binding) (cadr binding))
(define (binding-step binding) (caddr binding))

(define (clause-test clause) (car clause))
(define (clause-expression clause) (cdr clause))

(define (do->lambda exp)
  (define loop-name 'iter)
  (define (make-consequent body steps)
    (append body
            (list (append (list loop-name) steps))))
  (define (make-alternative body)
    (sequence->exp body))
  
  (let ((bindings (do-bindings exp))
        (clause (do-clause exp))
        (body (do-body exp)))
    (let ((vars (map binding-variable bindings))
          (inits (map binding-init bindings))
          (steps (map binding-step bindings))
          (test (clause-test clause))
          (else-exp (clause-expression clause)))

      (list
       (list 'lambda
             '()
             (list 'define (append (list loop-name) vars)
                   (list 'if test
                         (sequence->exp (make-alternative else-exp))
                         (sequence->exp (make-consequent body steps))))
             (append (list loop-name) inits))))))

(do->lambda
 '(do ((i 0 (+ i 1)))
    ((> i 3) (display (+ i 100)))
    (println i)))

(do ((i 0 (+ i 1)))
  ((> i 3) (display (+ i 100)))
  (println i))

;((lambda ()
;   (define (iter i)
;     (if (> i 3)
;         (display (+ i 100))
;         (begin (println i)
;                (iter (+ i 1)))))
;   (iter 0)))