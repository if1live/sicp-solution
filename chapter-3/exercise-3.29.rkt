; A or B = ~(~A and ~B)
; de morgan

(define or-gate-delay
  (+ inverter-delay and-gate-delay inverter-delay))

(define (or-gate a1 a2 output)
  (let ((not-a1 (make-wire))
        (not-a2 (make-wire))
        (not-a1-and-not-a2 (make-wire)))
    (inverter a1 not-a1)
    (inverter a2 not-a2)
    (and-gate not-a1 not-a2 not-a1-and-not-a2)
    (inverter not-a1-and-not-a2 output)
    'ok))
  