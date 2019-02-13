#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

; 1 2 4 8 16...
(define s (cons-stream 1 (add-streams s s)))

(stream-ref s 0)
(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)
(stream-ref s 4)


