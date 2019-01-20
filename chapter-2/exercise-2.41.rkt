#!/usr/bin/env racket
#lang scheme

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (enumerate-interval a b)
  (if (> a b)
      (list)
      (cons a (enumerate-interval (+ a 1) b))))

(define (check-sum? s triple)
  (let ((i (car triple))
        (j (cadr triple))
        (k (caddr triple)))
    (= (+ i j k) s)))

(define (triple-pairs n)
  (define (inner-j j)
    (map (lambda (i) (list i j)) (enumerate-interval 1 (- j 1))))
  (define (i-j-pairs k)
    (flatmap inner-j (enumerate-interval 1 (- k 1))))
  (define (i-j-k-pairs k)
    (map (lambda (i-j) (append i-j (list k))) (i-j-pairs k)))

  (flatmap i-j-k-pairs (enumerate-interval 1 n)))

(define (triple-sum-pairs n s)
  (filter (lambda (triple) (check-sum? s triple)) (triple-pairs n)))

(triple-sum-pairs 10 11)

