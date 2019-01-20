#!/usr/bin/env racket
#lang scheme

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
 (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
               (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (enumerate-interval a b)
  (if (> a b)
      (list)
      (cons a (enumerate-interval (+ a 1) b))))

(define (unique-pairs n)
  (define (inner j) 
    (map (lambda (i) (list i j)) (enumerate-interval 1 j)))
  (flatmap inner (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (let ((a (car pair))
        (b (cadr pair)))
    (list a b (+ a b))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 10)
  

