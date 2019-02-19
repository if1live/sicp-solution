#!/usr/bin/env racket
#lang racket

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause)
  (if (cond-=>? clause)
      (cddr clause)
      (cdr clause)))

(define (cond-=>? clause) (eq? (cadr clause) '=>))
(define (cond-recipient clause) (car (cond-actions clause)))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;(define (expand-clauses clauses)
;  (if (null? clauses)
;      'false
;      (let ((first (car clauses))
;            (rest (cdr clauses)))
;        (if (cond-else-clause? first)
;            (if (null? rest)
;                (sequence->exp (cond-actions first))
;                (error "ELSE clause isn't last -- COND-IF" clauses))
;            (make-if (cond-predicate first)
;                     (sequence->exp (cond-actions first))
;                     (expand-clauses rest))))))

(define (expand-clauses clauses)
  (define (make-simple first rest)
    (make-if (cond-predicate first)
             (sequence->exp (cond-actions first))
             (expand-clauses rest)))

  (define (make-recipient first rest)
    (list 'let
          (list (list 'test (cond-predicate first)))
          (make-if 'test
                   (list (cond-recipient first) 'test)
                   (expand-clauses rest))))             
  
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND-IF" clauses))
            (if (cond-=>? first)
                (make-recipient first rest)
                (make-simple first rest))))))

(cond->if
 '(cond (true 1)
        (false 2)
        (else 4)))

(cond->if
 '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
        (else false)))


;(cond ((assoc 'b '((a 1) (b 2))) => cadr)
;      (else false))
; -> convert ->
;(let ((value (assoc 'b '((a 1) (b 2))))
;  (if value
;      (cadr value)
;      false)))