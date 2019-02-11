#!/usr/bin/env racket
#lang sicp

(define (println x)
  (display x)
  (newline))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (forget-value! sum me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else (error "unknown request -- adder" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (forget-value! product me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else (error "unknown request -- multiplier" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "unknown request -- constant" request))
  (connect connector me)
  (set-value! connector value me))

(define (inform-about-value constraint)
  (constraint 'i-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'i-lost-my-value))

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else (error "unknown request -- probe" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?) (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "unknown operation -- connector" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (averager a b c)
  (let ((sum (make-connector))
        (x (make-connector)))
    (constant 0.5 x)
    (adder a b sum)
    (multiplier sum x c)
    'ok))

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(averager A B C)

(probe "a" A)
(probe "b" B)
(probe "c" C)

(set-value! A 10 'user)
(set-value! B 20 'user)

(forget-value! B 'user)
(set-value! C 40 'user)