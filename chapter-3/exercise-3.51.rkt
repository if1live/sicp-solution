#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))

;(define x (stream-enumerate-interval 0 10))

(stream-ref x 5)

(stream-ref x 7)
