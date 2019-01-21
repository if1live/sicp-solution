#!/usr/bin/env racket
#lang racket

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) set)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (append set1 set2))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

(define A '(2 3 5 7 11))
(define B '(1 3 5 7 9))

(adjoin-set 100 A)
(intersection-set A B)
(union-set A B)


