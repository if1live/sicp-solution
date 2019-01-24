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

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

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


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define (union-set set1 set2)
  (define (inner a b)
    (cond ((and (null? a) (null? b)) (list))
          ((and (not (null? a)) (null? b)) a)
          ((and (null? a) (not (null? b))) b)
          ((= (car a) (car b)) 
           (cons (car a) (inner (cdr a) (cdr b))))
          ((< (car a) (car b))
           (cons (car a) (inner (cdr a) b)))
          ((> (car a) (car b))
           (cons (car b) (inner a (cdr b))))))
  (list->tree (inner (tree->list-1 set1) (tree->list-1 set2))))

(define (intersection-set set1 set2)
  (define (inner a b)
    (cond ((and (null? a) (null? b)) (list))
          ((and (not (null? a)) (null? b)) a)
          ((and (null? a) (not (null? b))) b)
          ((= (car a) (car b)) (cons (car a) (inner (cdr a) (cdr b))))
          ((< (car a) (car b)) (inner (cdr a) b))
          ((> (car a) (car b)) (inner a (cdr b)))))
  (list->tree (inner (tree->list-1 set1) (tree->list-1 set2))))

(define (make-record key value) (cons key value))
(define (key record) (car record))

 
(define A (list->tree (list (make-record 1 10)
                            (make-record 3 30)
                            (make-record 5 50)
                            (make-record 7 70)
                            (make-record 9 90)
                            (make-record 11 110))))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (car set-of-records))) (car set-of-records))
        ((< given-key (key (car set-of-records))) (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (car set-of-records))) (lookup given-key (right-branch set-of-records)))))

(lookup 9 A)
(lookup 10 A)

