#!/usr/bin/env racket
#lang scheme

(define (reverse items)
  (define (iter remains results)
    (if (null? remains)
        results
        (iter (cdr remains) (cons (car remains) results))))
  (iter items '()))

;(define (deep-reverse items)
;  (cond ((null? items) '())
;        ((pair? items) 
;          (if (null? (cdr items))
;              (deep-reverse (car items))
;              (list (deep-reverse (cdr items)) (deep-reverse (car items)))))
;        (else items)))

(define (deep-reverse items)
  (cond ((null? items) '())
        ((pair? (car items))
            (append (deep-reverse (cdr items)) 
                    (list (deep-reverse (car items)))))
        (else
            (append (deep-reverse (cdr items)) 
                    (list (car items))))))

(define (deep-reverse2 items)
  (cond ((null? items) '())
        ((pair? items)
            (append (deep-reverse2 (cdr items))
                    (list (deep-reverse2 (car items)))))
        (else items)))

(define z (list 1 2))
(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2) (list 3 (list 4 5)) (list 6 7)))

(reverse x)

(println z)
(deep-reverse z)
(deep-reverse2 z)

(println x)
(deep-reverse x)
(deep-reverse2 x)

(println y)
(deep-reverse y)
(deep-reverse2 y)
