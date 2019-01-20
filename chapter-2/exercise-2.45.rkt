#!/usr/bin/env racket
#lang sicp
(#%require sicp-pict)

(define (split transform-a transform-b)
  (define (inner painter n)
    (if (= n 0)
        painter
        (let ((sub-painter (inner painter (- n 1))))
          (transform-a painter
                       (transform-b sub-painter sub-painter)))))
  inner)

(define right-split (split beside below))
(define up-split (split below beside))

; (paint (right-split einstein 2))
(paint (up-split einstein 2))