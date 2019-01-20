#!/usr/bin/env racket
#lang racket
(#%require sicp-pict)

;; vert
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

;; frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

;; segment
(define (make-segment start-vert end-vert)
  (list start-vert end-vert))

(define (start-segment s) (car s))
(define (end-segment s) (cadr s))


(define (frame-coord-map f)
  (lambda (v)
    (add-vert
     (origin-frame f)
     (add-vert (scale-vert (xcor-vert v) (edge1-frame f))
               (scale-vert (ycor-vert v) (edge2-frame f))))))
              
                             

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; mock
(define (draw-line a b)
  (display "start line: ")
  (println a)
  (display "end line: ")
  (println b))

(define sample-frame
  (make-frame (make-vert 10 20)
              (make-vert 1 0)
              (make-vert 0 2)))

; a.그림틀의 테두리를 그려주는 페인터
(define (border-painter frame)
  (let ((bl (make-vert 0 0))
        (br (make-vert 1 0))
        (tl (make-vert 0 1))
        (tr (make-vert 1 1)))
    (let ((segment-list (list (make-segment bl br)
                              (make-segment br tr)
                              (make-segment tr tl)
                              (make-segment tl bl))))
      ((segments->painter segment-list) frame))))

(display "border-painter\n")
(border-painter sample-frame)


; b.그림틀에서 마조부는 꼭짓점을 서로 연결하여 'X'를 그리는 페인터
(define (cross-painter frame)
  (let ((bl (make-vert 0 0))
        (br (make-vert 1 0))
        (tl (make-vert 0 1))
        (tr (make-vert 1 1)))
    (let ((segment-list (list (make-segment bl tr)
                              (make-segment br tl))))
      ((segments->painter segment-list) frame))))

(display "cross-painter\n")
(cross-painter sample-frame)

; c.그림틀의 모서리 가운데 점 네 개를 연결하여 다이아몬드 꼴을 그리는 페인터
(define (diamond-painter frame)
  (let ((b (make-vert 0.5 0))
        (t (make-vert 0.5 1))
        (l (make-vert 0 0.5))
        (r (make-vert 1 0.5)))
    (let ((segment-list (list (make-segment b r)
                              (make-segment r t)
                              (make-segment t l)
                              (make-segment l b))))
      ((segments->painter segment-list) frame))))

(display "diamond-painter\n")
(diamond-painter sample-frame)

; d.wave 페인터
