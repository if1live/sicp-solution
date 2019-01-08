#!/usr/bin/env racket
#lang scheme

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

(define (square x) (* x x))

(define (runtime) (current-inexact-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (report-not-prime elapsed-time)
  (display " NOT PRIME *** ")
  (display elapsed-time))

(define (report-search start-time)
  (newline)
  (display "total time: ")
  (display (- (runtime) start-time))
  (newline))

(define (search-for-primes start remain)
  (define (choice-start-value x)
    (if (= (remainder x 2) 0) (+ x 1) x))

  (define (check-finish remain) (= remain 0))

  (define (iter curr remain start-time)
    (if (check-finish remain)
        (report-search start-time)
        (if (equal? (timed-prime-test curr) #f)
            (not-found-prime-then-iter curr remain start-time)
            (found-prime-then-iter curr remain start-time))))

  (define (found-prime-then-iter curr remain start-time)
    (iter (+ curr 2) (- remain 1) start-time))

  (define (not-found-prime-then-iter curr remain start-time)
    (iter (+ curr 2) remain start-time))

  (iter (choice-start-value start) remain (runtime)))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
