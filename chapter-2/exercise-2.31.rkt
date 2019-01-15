#!/usr/bin/env racket
#lang scheme

(define (map proc items)
  (if (null? items)
      (list)
      (cons (proc (car items)) (map proc (cdr items)))))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square x) (* x x))

(define A
  (list 1
    (list 2 (list 3 4) 5)
    (list 6 7)))

(tree-map square A)
