#!/usr/bin/env racket
#lang scheme

(define (square x) (* x x))

(define (map proc items)
  (if (null? items) (list) (cons (proc (car items)) (map proc (cdr items)))))

(define (square-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? (car tree))) 
          (cons (square (car tree)) (square-tree (cdr tree))))
        (else 
          (cons (square-tree (car tree)) 
                (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree2 (car tree))
                    (square-tree2 (cdr tree))))))

(define (square-tree3 tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (square-tree3 sub-tree)
              (square sub-tree)))
       tree))


(define A
  (list 1
    (list 2 (list 3 4) 5)
    (list 6 7)))

(println A)
(square-tree A)
(square-tree2 A)
(square-tree3 A)

