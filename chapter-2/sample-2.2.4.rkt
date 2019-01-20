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

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((bottom (beside (bl painter) (br painter)))
          (top (beside (tl painter) (tr painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

; (paint (flipped-pairs einstein))
; (paint (square-limit einstein 2))

(define (make-vert x y) (list x y))
(define (xcor-vert v) (car v))
(define (ycor-vert v) (cadr v))

(define (add-vert a b)
  (make-vert (+ (xcor-vert a) (xcor-vert b))
             (+ (ycor-vert a) (ycor-vert b))))

(define (sub-vert a b)
  (make-vert (- (xcor-vert a) (xcor-vert b))
             (- (ycor-vert a) (ycor-vert b))))

(define (scale-vert s v)
  (make-vert (* s (xcor-vert v))
             (* s (ycor-vert v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vert
     (origin-frame frame)
     (add-vert (scale-vert (xcor-vert v) (edge1-frame frame))
               (scale-vert (ycor-vert v) (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

     
      