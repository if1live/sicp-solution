#!/usr/bin/env racket
#lang scheme

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate 
    (lambda (x y) (+ x y)) 
    0 
    (map (lambda (sub-tree)
           (cond ((null? sub-tree) 0)
                 ((pair? sub-tree) (count-leaves sub-tree))
                 (else 1)))
         t)))

(define A (cons (list 1 2) (list 3 4)))
(count-leaves A)
(count-leaves (list A A))
