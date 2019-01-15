#!/usr/bin/env racket
#lang scheme

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product (list 1 2 3) (list 4 5 6))

(define A (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(matrix-*-vector A (list 10 20 30 40))

(define (transpose mat)
  (accumulate-n (lambda (x y) (append (list x) y)) (list) mat))
(transpose (list (list 1 2)
                 (list 3 4)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

(define X (list (list 1 2)
                (list 3 4)))
(define Y (list (list 5 6)
                (list 7 8)))
(matrix-*-matrix X Y)
