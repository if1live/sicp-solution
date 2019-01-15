#!/usr/bin/env racket
#lang scheme

(define (subsets s)
  (if (null? s) 
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))
