#!/usr/bin/env racket
#lang scheme

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (square x) (* x x))

(println (fast-prime? 36 10))
(println (fast-prime? 37 10))

; carmichael number
(println (fast-prime? 561 10))
(println (fast-prime? 1105 10))
(println (fast-prime? 1729 10))
(println (fast-prime? 2465 10))
(println (fast-prime? 2821 10))
(println (fast-prime? 6601 10))


(define (fermat-full-test n)
  (define (check-it a)
    (if (= (expmod a n n) a) #t #f))

  (define (iter n a)
    (cond ((= a n) "finish")
          ((check-it a) (iter n (+ a 1)))
          (else "abort")))

  (iter n 2))

(println (fermat-full-test 36))
(println (fermat-full-test 37))

(println (fermat-full-test 561))
(println (fermat-full-test 1105))
(println (fermat-full-test 1729))
(println (fermat-full-test 2465))
(println (fermat-full-test 2821))
(println (fermat-full-test 6601))
