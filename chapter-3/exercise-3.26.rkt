#!/usr/bin/env racket
#lang sicp

(define (println x)
  (display x)
  (newline))

(define (make-node key value)
  (list key value '() '()))

(define (node-key node) (car node))
(define (node-value node) (cadr node))
(define (node-left node) (caddr node))
(define (node-right node) (cadddr node))

(define (set-node-value! node value) (set-car! (cdr node) value))
(define (set-node-left! node left) (set-car! (cddr node) left))
(define (set-node-right! node right) (set-car! (cdddr node) right))


(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr (local-table)))))
        (if record (cdr record) #f)))

    (define (assoc key node)
      (cond ((null? node) #f)
            ((equal? key (node-key node)) node)
            (else
             (let ((left-found (assoc key (node-left node)))
                   (right-found (assoc key (node-right node))))
               (cond (left-found left-found)
                     (right-found right-found)
                     (else #f))))))

    (define (insert-first! key value)
      (let ((new-node (make-node key value)))
        (set-cdr! local-table new-node)))

    (define (insert-normal! key value node)
      (let ((curr-key (node-key node))
            (new-node (make-node key value)))
        (cond ((= curr-key key) (set-node-value! node value))
              ((> curr-key key)
               (if (null? (node-left node))
                   (set-node-left! node new-node)
                   (insert-normal! key value (node-left node))))
              ((< curr-key key)
               (if (null? (node-right node))
                   (set-node-right! node new-node)
                   (insert-normal! key value (node-right node)))))))                   

    (define (insert! key value)
      (if (null? (cdr local-table))
          (insert-first! key value)
          (insert-normal! key value (cdr local-table)))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'dump) local-table)
            (else (error "make-table -- unknown command" m))))
    
    dispatch))

(define (insert! key value table) ((table 'insert!) key value))
(define (lookup key table) ((table 'lookup) key))

(define t (make-table))
(println (t 'dump))

(insert! 10 'd t)
(println (t 'dump))

(insert! 5 'b t)
(println (t 'dump))

(insert! 8 'c t)
(println (t 'dump))

(insert! 1 'test t)
(println (t 'dump))

(insert! 15 'e t)
(println (t 'dump))

(insert! 1 'a t)
(println (t 'dump))