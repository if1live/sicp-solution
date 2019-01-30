#!/usr/bin/env racket
#lang racket

(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (incorrect-passwd . args)
    "incorrect password")

  (define (dispatch pw m)
    (if (eq? pw passwd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "unknown request -- make-account" m)))
        incorrect-passwd))
  dispatch)
    

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
