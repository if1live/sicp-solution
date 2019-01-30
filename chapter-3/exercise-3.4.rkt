#!/usr/bin/env racket
#lang racket

(define (make-account balance passwd)
  (let ((passwd-check-count 0))
    (define (reset-passwd-check)
      (set! passwd-check-count 0))
    
    (define (withdraw amount)
      (reset-passwd-check)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "insufficient funds"))
    
    (define (deposit amount)
      (reset-passwd-check)
      (set! balance (+ balance amount))
      balance)

    (define (incorrect-passwd . args)
      (set! passwd-check-count (+ passwd-check-count 1))
      (if (> passwd-check-count 7)
          "call-the-cage"
          "incorrect password"))

    (define (dispatch pw m)
      (if (eq? pw passwd)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "unknown request -- make-account" m)))
          incorrect-passwd))
    dispatch))
    

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

(map (lambda (x) ((acc 'some-other-password 'deposit) 50))
     (make-list 8 10))