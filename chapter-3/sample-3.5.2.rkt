#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
(stream-ref no-sevens 100)

; (define (fibgen a b)
;   (cons-stream a (fibgen b (+ a b))))
; (define fibs (fibgen 0 1))

(define fibs
  (cons-stream
    0
    (cons-stream
      1
      (add-streams (stream-cdr fibs)
                   fibs))))

(stream-ref fibs 5)


(define (sieve s)
  (cons-stream
    (stream-car s)
    (sieve (stream-filter
            (lambda (x) (not (divisible? x (stream-car s))))
            (stream-cdr s)))))
; (define primes (sieve (integers-starting-from 2)))
(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (square x) (* x x))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))


(stream-ref primes 50)

(stream-ref integers 5)

(define double (cons-stream 1 (scale-stream double 2)))


