#!/usr/bin/env racket
#lang scheme

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(println (a-plus-abs-b 1 -2))
(println (a-plus-abs-b 1 2))
