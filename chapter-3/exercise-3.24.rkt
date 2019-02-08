#!/usr/bin/env racket
#lang sicp

(define (println x)
  (display x)
  (newline))

; (define (lookup key table)
;   (let ((record (assoc key (cdr table))))
;     (if record
;         (cdr record)
;         false)))

; (define (assoc key records)
;   (cond ((null? records) #f)
;         ((equal? key (caar records)) (car records))
;         (else (assoc key (cdr records)))))

; (define (insert! key value table)
;   (let ((record (assoc key (cdr table))))
;     (if record
;         (set-cdr! record value)
;         (set-cdr! table
;                   (cons (cons key value) (cdr table))))))

; (define (make-table)
;   (list '*table*))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            #f)))

    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "make-table -- unknown command" m))))

    dispatch))

(define (lookup key table) ((table 'lookup) key))
(define (insert! key value table) ((table 'insert!) key value))

(define (simple-same-key? a b)
  (equal? a b))

(define (float-same-key? a b)
  (< (abs (- a b)) 0.01))    

(define t (make-table simple-same-key?))
(insert! 'a 1 t)
(insert! 'b 2 t)
(insert! 'c 3 t)
(println (lookup 'a t))
(println (lookup 'd t))


(define t2 (make-table float-same-key?))
(insert! 3.0001 'a t2)
(println (lookup 3 t2))
(println (lookup 3.1 t2))
