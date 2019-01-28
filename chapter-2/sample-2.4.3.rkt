#!/usr/bin/env racket
#lang racket

;; http://uents.hatenablog.com/entry/sicp/020-ch2.4.3.1.md
(define *op-table* (make-hash))

(define (put op type item)
  (if (not (hash-has-key? *op-table* op))
        (hash-set! *op-table* op (make-hash))
              true)
    (hash-set! (hash-ref *op-table* op) type item))

(define (get op type)
  (define (not-found . msg)
      (display msg (current-error-port))
          (display "\n")
              false)
    (if (hash-has-key? *op-table* op)
          (if (hash-has-key? (hash-ref *op-table* op) type)
                    (hash-ref (hash-ref *op-table* op) type)
                              (not-found "Bad key -- TYPE" type))
                (not-found "Bad key -- OPERATION" op)))

(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z)
    (sqrt (+ (square (real-part z)) 
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) 
          (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
    (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
    (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

(install-rectangular-package)
(install-polar-package)

; shared
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular)  x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define A (make-from-real-imag 10 20))
(define B (make-from-mag-ang 5 1.57))

(add-complex A B)
(sub-complex A B)
(mul-complex A B)
(div-complex A B)

(define C (add-complex A B))
(println (real-part C))
(println (imag-part C))
(println (magnitude C))
(println (angle C))
