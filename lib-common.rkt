#!/usr/bin/env racket
#lang racket

(define (square x) (* x x))

;; http://uents.hatenablog.com/entry/sicp/020-ch2.4.3.1.md
(define *op-table* (make-hash))

(define (put op type item)
  (if (not (hash-has-key? *op-table* op))
        (hash-set! *op-table* op (make-hash))
              true)
    (hash-set! (hash-ref *op-table* op) type item))

(define (get op type)
    (if (hash-has-key? *op-table* op)
          (if (hash-has-key? (hash-ref *op-table* op) type)
                    (hash-ref (hash-ref *op-table* op) type)
                              #f)
                #f))


(provide (all-defined-out))
