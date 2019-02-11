#!/usr/bin/env racket
#lang sicp

(define the-stream-empty '())
(define (stream-null? s) (null? s))

;(define (delay exp) (lambda () exp))
;(define (force delayed-object) (delayed-object))
;(define (cons-stream a b) (cons a (delay b)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-stream-empty
;      (cons-stream (proc (stream-car s))
;                   (stream-map proc (stream-cdr s)))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
                    (apply stream-map
                    (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

; (define integers (integers-starting-from 1))

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))


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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))


