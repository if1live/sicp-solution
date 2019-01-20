#!/usr/bin/env racket
#lang scheme

; default library
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op initial (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (enumerate-interval a b)
  (if (> a b)
      (list)
      (cons a (enumerate-interval (+ a 1) b))))

; (accumulate + 0 (list 1 2 3))
; (enumerate-interval 2 4)

; provided code
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter 
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (new-row)
              (map (lambda (rest-of-queens)
                     (adjoin-position new-row k rest-of-queens))
                   (queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

; solution
(define (make-pt row col) (list row col))
(define (pt-row p) (car p))
(define (pt-col p) (cadr p))

(define empty-board (list))

(define (safe? k positions)
  (let ((kth-queen (car positions))
        (rest-queens (cdr positions)))
    (accumulate 
      (lambda (a b) (and a b))
      #t
      (map (lambda (other-queen) (safe-queen? kth-queen other-queen)) rest-queens))))

(define (safe-queen? a b)
  (let ((ax (pt-col a))
        (ay (pt-row a))
        (bx (pt-col b))
        (by (pt-row b)))
    (not (or (= ax bx)
             (= ay by)
             (= (abs (- ax bx)) (abs (- ay by)))))))

(define (adjoin-position new-row k rest-of-queens)
  (append (list (make-pt new-row k)) rest-of-queens))
 
;(queens 0) 
;(queens 1) 
(queens 3)
(queens 5)
(queens 6)
(queens 7)

