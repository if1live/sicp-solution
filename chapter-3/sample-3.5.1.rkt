#!/usr/bin/env racket
#lang racket

(require "lib-stream.rkt")

(display-stream
  (stream-filter even? (stream-enumerate-interval 10 20)))
