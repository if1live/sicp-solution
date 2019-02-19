#!/usr/bin/env racket
#lang racket

(require "./lib-evaluator.rkt")
(require compatibility/mlist)

; set-car!를 쓰기 귀
(my-eval (list->mlist '(* 5 5)) the-global-environment)
(my-eval (mcons '* (mlist 5 5)) the-global-environment)