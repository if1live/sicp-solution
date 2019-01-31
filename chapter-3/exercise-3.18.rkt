#!/usr/bin/env racket
#lang sicp

(define (println x)
  (display x)
  (newline))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define A (list 'a 'b 'c))
(define B (make-cycle (list 'a 'b 'c)))

(define C (list 'a 'b 'c))
(set-car! C (last-pair C))

(println A)
(println B)
(println C)


(define (pair-in-trail? trail x)
  (cond ((null? trail) #f)
        ((eq? (car trail) x) #t)
        (else (pair-in-trail? (cdr trail) x))))

(define (cycle? x)
  (define (iter remain trail)
    (let ((next-trail (cons remain trail)))
      (cond ((not (pair? remain)) #f)
            ((null? remain) #f)
            ((pair-in-trail? trail remain) #t)
            (else 
             (or (and (pair? (car remain)) 
                      (iter (car remain) next-trail))
                 (and (pair? (cdr remain))
                      (iter (cdr remain) next-trail)))))))
  (iter x '()))

(cycle? A)
(cycle? B)
(cycle? C)
