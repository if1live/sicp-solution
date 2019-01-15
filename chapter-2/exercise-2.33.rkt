#!/usr/bin/env racket
#lang scheme

(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
          (cons (car sequence)
                (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))

;(enumerate-interval 2 7)
;(filter odd? (list 1 2 3 4))
;(accumulate * 1 (list 2 3 4 5 6))
;(enumerate-tree (list 1 (list 3 (list 10 20) 4) 4))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))

(map (lambda (x) (+ 1 x)) (list 1 2 3))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2) (list 3 4))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 2 3))

