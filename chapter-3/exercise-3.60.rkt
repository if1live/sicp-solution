#!/usr/bin/env racket
#lang sicp

(define (println x)
  (display x)
  (newline))

(define the-empty-stream '())
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


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))

; exercise-3.59.a
(define (integrate-series s)
  (define (make-coef a n)
    (/ a n))
  (stream-map make-coef s integers))

; exercise-3.59.b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map (lambda (x) (- x)) sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))
                            (add-streams (stream-map (lambda (x) (* x (stream-car s2))) (stream-cdr s1))
                                         (stream-map (lambda (x) (* x (stream-car s1))) (stream-cdr s2))))))
                            

(define A (mul-series cosine-series cosine-series))
(define B (mul-series sine-series sine-series))
(define C (add-streams A B))
;(define C (mul-series integers integers))

(stream-ref C 0)
(stream-ref C 1)
(stream-ref C 2)
(stream-ref C 3)
(stream-ref C 4)