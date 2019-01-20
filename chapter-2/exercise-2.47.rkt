#!/usr/bin/env racket
#lang scheme

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define A (make-frame "origin" "edge1" "edge2"))
(origin-frame A)
(edge1-frame A)
(edge2-frame A)


(define (make-frame-alter origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-alter f) (car f))
(define (edge1-frame-alter f) (cadr f))
(define (edge2-frame-alter f) (cddr f))

(define B (make-frame-alter "origin" "edge1" "edge2"))

(origin-frame-alter B)
(edge1-frame-alter B)
(edge2-frame-alter B)