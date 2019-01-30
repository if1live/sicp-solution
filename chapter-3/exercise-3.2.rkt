#!/usr/bin/env racket
#lang racket

(define (make-monitored f)
  (let ((count 0))
    (define (mf args)
      (set! count (+ count 1))
      (apply f args))
    
    (define (reset-count)
      (set! count 0))
    
    (define (dispatch . args)
      (cond ((and (= (length args) 1) (eq? (car args) 'how-many-calls?))
             count)
            ((and (= (length args) 1) (eq? (car args) 'reset-count))
             (reset-count))
            (else (mf args))))
    
    dispatch))

(define s (make-monitored sqrt))

(s 100)
(s 'how-many-calls?)

(s 25)
(s 'how-many-calls?)

(s 'reset-count)
(s 'how-many-calls?)

(define add (make-monitored +))
(add 1 2)
(add 'how-many-calls?)