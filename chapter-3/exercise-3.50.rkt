#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(display-stream
  (stream-map + (stream-enumerate-interval 10 20)
                (stream-enumerate-interval 100 120)))
