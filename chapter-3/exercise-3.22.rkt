#!/usr/bin/env racket
#lang sicp

(define (println x)
  (display x)
  (newline))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?) 
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "delete! called with an empty queue"))
            (else 
             (set-front-ptr! (cdr front-ptr)))))

    (define (front-queue)
      (if (empty-queue?)
          (error "front called with an empty queue")
          (car front-ptr)))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else (error "undefined operation -- make-queue" m))))
    dispatch))

(define (empty-queue? queue) ((queue 'empty-queue?)))
(define (front-queue queue) ((queue 'front-queue)))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) ((queue 'delete-queue!)))

(define (print-queue queue)
  (cond ((empty-queue? queue)
         (display "empty queue")
         (newline))
        (else
         (display "front=")
         (display (front-queue queue))
         (newline))))

(define q1 (make-queue))
(print-queue q1)

(insert-queue! q1 'a)
(print-queue q1)

(insert-queue! q1 'b)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)


