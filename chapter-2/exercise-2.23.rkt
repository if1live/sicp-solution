#!/usr/bin/env racket
#lang scheme

(define (for-each f items)
  (define (inner item remains)
    (f item)
    (for-each f remains))

  (if (null? items)
      #t
      (inner (car items) (cdr items))))


(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

