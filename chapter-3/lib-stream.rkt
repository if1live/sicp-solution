#!/usr/bin/env racket
#lang racket

(require "../lib-common.rkt")

; https://stackoverflow.com/questions/13998388/running-code-from-sicp-section-3-5-4-with-drracket
(define the-empty-stream '())

(define (stream-null? stream) (null? stream))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.5.1 스트림과 (계산을) 미룬 리스트
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-stream-empty
;      (cons-stream (proc (stream-car s))
;                   (stream-map proc (stream-cdr s)))))

;;;;;;;;;;;;;;;;
;; exercise-3.50
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

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
          (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

;(define (delay exp) (lambda () exp))
;(define (force delayed-object) (delayed-object))
;(define (cons-stream a b) (cons a (delay b)))

;;;;;;;;;;;;;;;;;;;
;; 3.5.2 무한 스트림
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
; (define integers (integers-starting-from 1))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
; (define double (cons-stream 1 (scale-stream double 2)))

;;;;;;;;;;;;;;;;
;; exercise-3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 쌍으로 이루어진 무한 스트림

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


;;;;;;;;;;;;;;;;;;;;;;
;; 3.5.3 스트림 패러다임
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(provide (all-defined-out))
