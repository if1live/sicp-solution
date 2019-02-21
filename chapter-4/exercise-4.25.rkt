#!/usr/bin/env racket
#lang racket

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

; 메모리 부족으로 프로그램이 죽는다
; 인자를 끝없이 계산하는게 문제
;(define (factorial n)
;  (unless (= n 1)
;    (* n (factorial (- n 1)))
;    1))

(factorial 5)