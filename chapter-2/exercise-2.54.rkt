#!/usr/bin/env racket
#lang racket

(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((and (null? a) (not (null? b))) #f)
        ((and (not (null? a)) (null? b)) #f)
        ((and (pair? a) (pair? b))
            (and (equal? (car a) (car b))
                 (equal? (cdr a) (cdr b))))
        ((and (not (pair? a)) (not (pair? b)))
            (eq? a b))
        (else #f)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))


