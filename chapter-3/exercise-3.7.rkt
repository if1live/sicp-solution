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

(define (make-joint account parent-passwd passwd)
  (define (incorrect-passwd . args)
    "incorrect password")
  (define (dispatch pw m)
    (if (or (eq? pw parent-passwd) (eq? pw passwd))
        (lambda (amount) ((account parent-passwd m) amount))
        incorrect-passwd))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 10)
((paul-acc 'foo 'withdraw) 20)
((paul-acc 'rosebud 'withdraw) 20)