#!/usr/bin/env racket
#lang racket

; mock
(define (no-operands? exps) (null? exps))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (true? x) (not (false? x)))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (eval exp env)
  (display exp)
  (display "  ")
  exp)

(define (eval-and exp env)
  (define (eval-and-common exps env)
    (if (no-operands? exps)
        'true
        (let ((first (eval (first-operand exps) env)))
          (if (false? first)
              'false
              (eval-and-common (rest-operands exps) env)))))
  (eval-and-common (cdr exp) env))

(define (eval-or exp env)
  (define (eval-or-common exps env)
    (if (no-operands? exps)
        'false
        (let ((first (eval (first-operand exps) env)))
          (if (true? first)
             'true
             (eval-or-common (rest-operands exps) env)))))
  (eval-or-common (cdr exp) env))

(eval-and (list 'and #t #f 1) '())
(eval-and (list 'and) '())

(eval-or (list 'or #f #t 0) '())
(eval-or (list 'or) '())


(define (and->if exp)
  (expand-and (cdr exp)))

(define (expand-and ops)
  (if (null? ops)
      'true
      (let ((first (car ops))
            (rest (cdr ops)))
        (make-if first
                 (expand-and rest)
                 'false))))

(define (or->if exp)
  (expand-or (cdr exp)))

(define (expand-or ops)
  (if (null? ops)
      'false
      (let ((first (car ops))
            (rest (cdr ops)))
        (make-if first
                 'true
                 (expand-or rest)))))


(and->if (list 'and #t #f 1))
(and->if (list 'and))

(or->if (list 'or #f #t 0))
(or->if (list 'or))


