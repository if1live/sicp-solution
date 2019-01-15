#!/usr/bin/env racket
#lang scheme

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m) (car m))
(define (right-branch m) (cadr m))
(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))

(define (branch-weight b)
  (let ((structure (branch-structure b)))
    (cond ((pair? structure) (total-weight structure))
          (else structure))))

(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(define A
  (make-mobile (make-branch 4 10)
               (make-branch 8 (make-mobile (make-branch 2 20)
                                           (make-branch 6 15)))))

(define B
  (make-mobile (make-branch 4 70)
               (make-branch 8 (make-mobile (make-branch 2 20)
                                           (make-branch 6 15)))))
(println A)
(total-weight A)
(println B)
(total-weight B)


(define (branch-torque b)
  (* (branch-length b)
     (branch-weight b)))

(define (balanced-mobile? m)
  (= (branch-torque (left-branch m))
     (branch-torque (right-branch m))))

(balanced-mobile? A)
(balanced-mobile? B)

