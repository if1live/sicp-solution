#!/usr/bin/env racket
#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))


(define (display-list-tree tree)
  (println tree)
  (println (tree->list-1 tree))
  (println (tree->list-2 tree))
  (newline))

(define A (foldl adjoin-set '() '(7 3 5 1 9 11)))
(define B (foldl adjoin-set '() '(3 1 7 5 9 11)))
(define C (foldl adjoin-set '() '(5 3 1 9 7 11)))

(display-list-tree A)
(display-list-tree B)
(display-list-tree C)

(define D (foldl adjoin-set '() '(1 2 3 4 5)))
(display-list-tree D)

(define E (foldl adjoin-set '() '(5 4 3 2 1)))
(display-list-tree E)

