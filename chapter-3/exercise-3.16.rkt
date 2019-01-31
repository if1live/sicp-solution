#!/usr/bin/env racket
#lang sicp

(define (println x)
  (display x)
  (newline))

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; 3
(define (three)
  (define A (list 'a 'b 'c))
  A)
(println (count-pairs (three)))

; 4
(define four-a (list 'a))
(define four-b (cons 'b four-a))
(define four-X (cons four-b four-a))
(define (four) four-X)
(println (count-pairs (four)))

; 5
(define five-a (list 'a 'b))
(define (five) (cons five-a five-a))
(println (count-pairs (five)))

; 7
; http://community.schemewiki.org/?sicp-ex-3.16
(define seven-a (list 'a))
(define seven-b (cons seven-a seven-a))
(define (seven) (cons seven-b seven-b))
(println (count-pairs (seven)))

; loop
(define loop-a (list 'a 'b))
(define loop-X (cons loop-a loop-a))
(set-cdr! loop-X loop-X)
(define (loop) loop-X)
(println (loop))
; (println (count-pairs (loop)))
