#!/usr/bin/env racket
#lang sicp
(#%require sicp-pict)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((right (right-split painter (- n 1))))
        (beside painter (below right right)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1))))
        (below painter (beside up up)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (right-bottom (below right right)))
          (beside (below painter top-left)
                  (below right-bottom (corner-split painter (- n 1))))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; (paint (corner-split einstein 2))
(paint (square-limit einstein 2))
; (paint (up-split einstein 2))
; (paint (right-split einstein 2))