#!/usr/bin/env racket
#lang racket/gui
(#%require sicp-pict)

; https://ericscrivner.me/2015/05/the-sicp-picture-language-in-racket/
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))
(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

;; helper func
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start-coord-map ((frame-coord-map frame) (segment-start segment)))
             (end-coord-map ((frame-coord-map frame) (segment-end segment))))
       (line
        (make-posn (vector-xcor start-coord-map) (vector-ycor start-coord-map))
        (make-posn (vector-xcor end-coord-map) (vector-ycor end-coord-map)))))
     segment-list)))
 
(define simple-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 0))
    (make-segment (make-vect 0 0) (make-vect 0 0.2))
    (make-segment (make-vect 1 0) (make-vect 0 0.2))
    (make-segment (make-vect 0 0) (make-vect 0.5 0))
    (make-segment (make-vect 0 0) (make-vect 0 1))
    (make-segment (make-vect 0.5 0) (make-vect 0 1)))))

(define marker-painter
  (let ((x 0.05))
    (segments->painter
     (list
      (make-segment (make-vect (- x) (- x)) (make-vect x x))
      (make-segment (make-vect (- x) x) (make-vect x (- x)))))))
 
(define unit-frame (make-frame (make-vect 250 250) (make-vect 100 0) (make-vect 0 -100)))
(marker-painter unit-frame)
; (simple-painter unit-frame)

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (vector-sub (m corner1) new-origin)
                     (vector-sub (m corner2) new-origin)))))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0 0)
                              (make-vect 1 0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1 0.5)
                              (make-vect 0 1))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))  

((below simple-painter simple-painter) unit-frame)

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0 0)
                              split-point
                              (make-vect 0 1)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1 0)
                              (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (rot270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (rot90 painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))

; ((beside simple-painter simple-painter) unit-frame)

((rot270 (beside (rot90 simple-painter) (rot90 simple-painter))) unit-frame)