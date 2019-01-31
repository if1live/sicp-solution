#!/usr/bin/env racket
#lang sicp

(define (println x)
  (display x)
  (newline))
    
(define (set-has-pair? set x)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (set-has-pair? (cdr set) x))))


(define (count-pairs-mutable x)
  (let ((set '()))
    (define (add-set! x)
      (set! set (cons x set)))

    (define (inner target)
      (cond ((not (pair? target)) #f)
            ((set-has-pair? set target) #f)
            (else 
             (begin (add-set! target)
                    (inner (car target))
                    (inner (cdr target))
                    #t))))
    (inner x)
    (length set)))


(define (count-pairs-immutable x)
  (define (inner target set)
    (cond ((not (pair? target)) set)
          ((set-has-pair? set target) set)
          (else
           (let ((set-1 (cons target set)))
             (let ((set-2 (inner (car target) set-1)))
               (inner (cdr target) set-2))))))
  (length (inner x '())))

(define count-pairs count-pairs-mutable)
; (define count-pairs count-pairs-immutable)

; (define (count-pairs x)
;   (if (not (pair? x))
;       0
;       (+ (count-pairs (car x))
;          (count-pairs (cdr x))
;          1)))

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

; TODO 7

; loop
(define loop-a (list 'a 'b))
(define loop-X (cons loop-a loop-a))
(set-cdr! loop-X loop-X)
(define (loop) loop-X)
; (println (loop))
(println (count-pairs (loop)))
