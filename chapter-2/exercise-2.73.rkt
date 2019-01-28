#!/usr/bin/env racket
#lang racket

(define *op-table* (make-hash))

(define (put op type item)
  (if (not (hash-has-key? *op-table* op))
        (hash-set! *op-table* op (make-hash))
              true)
    (hash-set! (hash-ref *op-table* op) type item))

(define (get op type)
  (define (not-found . msg)
      (display msg (current-error-port))
          (display "\n")
              false)
    (if (hash-has-key? *op-table* op)
          (if (hash-has-key? (hash-ref *op-table* op) type)
                    (hash-ref (hash-ref *op-table* op) type)
                              (not-found "Bad key -- TYPE" type))
                (not-found "Bad key -- OPERATION" op)))


(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? a b) 
  (and (variable? a) (variable? b) (eq? a b)))
(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv-sum operands var)
  (if (= (length operands) 1)
      (deriv (car operands) var)
      (make-sum (deriv (car operands) var)
                (deriv-sum (cdr operands) var))))

(define (deriv-product operands var)
  (if (= (length operands) 2)
      (make-sum 
        (make-product (deriv (car operands) var)
                      (cadr operands))
        (make-product (car operands)
                      (deriv (cadr operands) var)))
      (make-sum
        (make-product (deriv (car operands) var)
                      (cdr operands))
        (make-product (car operands)
                      (deriv-product (cdr operands) var)))))

(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))


(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y z w) 'x)
