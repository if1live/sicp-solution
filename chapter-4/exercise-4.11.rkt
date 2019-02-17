#!/usr/bin/env racket
#lang sicp

(define (println x)
  (display x)
  (newline))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-binding var val) (cons var val))
(define (binding-var b) (car b))
(define (binding-val b) (cdr b))
(define (set-binding-val! b v) (set-cdr! b v))

(define (make-frame bindings) bindings)
(define (frame-bindings f) f)
(define (add-binding-to-frame! var val frame)
  ;(set-cdr! frame (cons (car frame) (cdr frame)))
  (set-cdr! frame frame)
  (set-car! frame (make-binding var val)))

(define (make-bindings vars vals)
  (if (null? vars)
      '()
      (cons (make-binding (car vars) (car vals))
            (make-bindings (cdr vars) (cdr vals)))))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame (make-bindings vars vals)) base-env)
      (if (< (length vars) (length vals))
          (error "too many arguments supplied" vars vals)
          (error "too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (binding-var (car bindings)))
             (binding-val (car bindings)))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-bindings frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (binding-var (car bindings)))
             (set-binding-val! (car bindings) val))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable -- set!" var)
        (let ((frame (first-frame env)))
          (scan (frame-bindings frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((null? bindings)
             (add-binding-to-frame! var val frame))
            ((eq? var (binding-var (car bindings)))
             (set-binding-val! (car bindings) val))
            (else (scan (cdr bindings)))))
    (scan (frame-bindings frame))))


; expected output
; 1
; 10
; 0
; 30
; 100

(define a (extend-environment '(foo bar) '(1 2) the-empty-environment))
(define b (extend-environment '(foo spam) '(10 0) a))

(lookup-variable-value 'foo a)
(lookup-variable-value 'foo b)

(lookup-variable-value 'spam b)
(set-variable-value! 'spam 30 b)
(lookup-variable-value 'spam b)

; (lookup-variable-value 'extra b)
(define-variable! 'extra 100 b)
(lookup-variable-value 'extra b)
