#!/usr/bin/env racket
#lang scheme

(define (fringe tree)
  (define (iter items results)
    (cond ((null? items) results)
          ((pair? (car items)) 
              (append (iter (car items) results)
                      (iter (cdr items) (list))))
          (else 
              (iter (cdr items) (append results (list (car items)))))))
  (iter tree (list)))

(define (fringe2 tree)
  (define (iter items results)
    (cond ((null? items) results)
          ((not (pair? items)) (append results (list items)))
          (else (append (iter (car items) results)
                        (iter (cdr items) (list))))))
  (iter tree (list)))        

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(fringe (list x x))

(fringe2 x)
(fringe2 (list x x))
