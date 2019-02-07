#!/usr/bin/env racket
#lang sicp

(define (println x)
  (display x)
  (newline))

(define (make-node item)
  (list '() '() item))

(define (node-next node) (car node))
(define (node-prev node) (cadr node))
(define (node-item node) (caddr node))
(define (set-node-next! node next) (set-car! node next))
(define (set-node-prev! node prev) (set-car! (cdr node) prev))

(define (make-deque)
  (cons '() '()))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "front called with an empty deque" deque)
      (node-item (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "rear called with an empty deque" deque)
      (node-item (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-node (make-node item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node))
          (else
           (set-node-prev! (front-ptr deque) new-node)
           (set-node-next! new-node (front-ptr deque))
           (set-front-ptr! deque new-node)))))

(define (rear-insert-deque! deque item)
  (let ((new-node (make-node item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node))
          (else
           (set-node-next! (rear-ptr deque) new-node)
           (set-node-prev! new-node (rear-ptr deque))
           (set-rear-ptr! deque new-node)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "front-delete called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-front-ptr! deque (node-next (front-ptr deque)))
         (set-node-prev! (front-ptr deque) '()))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "rear-delete called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-rear-ptr! deque (node-prev (rear-ptr deque)))
         (set-node-next! (rear-ptr deque) '()))))

(define (print-deque deque)
  (cond ((empty-deque? deque)
         (println "empty deque"))
        (else
         (display "front=")
         (display (front-deque deque))
         (display " / ")
         (display "rear=")
         (println (rear-deque deque)))))

(define d (make-deque))
(print-deque d)

(front-insert-deque! d 2)
(print-deque d)

(rear-insert-deque! d 3)
(print-deque d)

(front-insert-deque! d 1)
(print-deque d)

(rear-insert-deque! d 4)
(print-deque d)

(rear-delete-deque! d)
(print-deque d)

(front-delete-deque! d)
(print-deque d)

(rear-delete-deque! d)
(print-deque d)

(front-delete-deque! d)
(print-deque d)



