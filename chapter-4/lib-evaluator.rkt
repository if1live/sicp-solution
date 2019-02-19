#!/usr/bin/env racket
#lang racket

; set-car!, set-cdr! 때문에 추가
; 이게 들어간면 cons가 아니라 mcons 계열로 바뀐다
; (require compatibility/mlist)
; list->mlist 이용해서 변환 가능
(require (planet neil/sicp:1:13))

; 내장 apply
(define apply-in-underlying-scheme apply)

; 4.1.1
(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (my-eval (cond->if exp) env))
        ((and? exp) (my-eval (and->if exp) env))
        ((or? exp) (my-eval (or->if exp) env))
        ((let? exp) (my-eval (let->combination exp) env))
        ((let*? exp) (my-eval (let*->nested-lets exp) env))
        ((do? exp) (my-eval (do->lambda exp) env))
        ((application? exp)
         (my-apply (my-eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else
         (error "unknown expression type -- MY-EVAL" exp))))

; 내장된 apply를 덮어씌우지 않도록 이름 변경
(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error "unknown procedure type -- MY-APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else (my-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (my-eval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; basic + exercise-4.5
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause)
  (if (cond-=>? clause)
      (cddr clause)
      (cdr clause)))
(define (cond-=>? clause) (eq? (cadr clause) '=>))
(define (cond-recipient clause) (car (cond-actions clause)))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;(define (expand-clauses clauses)
;  (if (null? clauses)
;      'false
;      (let ((first (car clauses))
;            (rest (cdr clauses)))
;        (if (cond-else-clause? first)
;            (if (null? rest)
;                (sequence->exp (cond-actions first))
;                (error "ELSE clause isn't last -- COND-IF" clauses))
;            (make-if (cond-predicate first)
;                     (sequence->exp (cond-actions first))
;                     (expand-clauses rest))))))

; exercise-4.5
(define (expand-clauses clauses)
  (define (make-simple-if first rest)
    (make-if (cond-predicate first)
             (sequence->exp (cond-actions first))
             (expand-clauses rest)))

  (define (make-recipient-if first rest)
    (list 'let
          (list (list 'test (cond-predicate first)))
          (make-if 'test
                   (list (cond-recipient first) 'test)
                   (expand-clauses rest))))

  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND-IF" clauses))
            (if (cond-=>? first)
                (make-recipient-if first rest)
                (make-simple-if first rest))))))

;(cond (true 1)
;      (false 2)
;      (else 4)))

;(cond ((assoc 'b '((a 1) (b 2))) => cadr)
;      (else false)))
;;;;;;;;;;;;;;

; exercise-4.4
(define (and? exp) (tagged-list? exp 'and))
(define (and->if exp) (expand-and (cdr exp)))

(define (expand-and ops)
  (if (null? ops)
      'true
      (let ((first (car ops))
            (rest (cdr ops)))
        (make-if first
                 (expand-and rest)
                 'false))))

(define (or? exp) (tagged-list? exp 'or))
(define (or->if exp) (expand-or (cdr exp)))

(define (expand-or ops)
  (if (null? ops)
      'false
      (let ((first (car ops))
            (rest (cdr ops)))
        (make-if first
                 'true
                 (expand-or rest)))))

; (and ((lambda () (display 'first) false))  ((lambda () (display 'second) true)))
; (or ((lambda () (display 'first) true)) ((lambda () (display 'second) false)))
; (and)
; (or)
;;;;;;;;;;;;;;

; exercise-4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let-body exp) (cddr exp))
(define (let-clauses exp) (cadr exp))
(define (let-variable clause) (car clause))
(define (let-expression clause) (cadr clause))
(define (make-lambda-with-call vars body exps)
  (append (list (list 'lambda vars body)) exps))

(define (simplelet->combination exp)
  (let ((clauses (let-clauses exp))
        (body (let-body exp)))
    (let ((vars (map let-variable clauses))
          (exps (map let-expression clauses)))
      (make-lambda-with-call vars (make-begin body) exps))))
; (let ((a 1) (b 2) (c 3))  (display a) (+ a b c))
;;;;;;;;;;;;;;

; exercise-4.7
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  (define (make-let var exp body)
    (cond ((let? body) (list 'let (list (list var exp)) body))
          ((= (length body) 1) (list 'let (list (list var exp)) (car body)))
          (else (append '(let) (list (list (list var exp))) body))))

  (define (make-inner-let vars exps body)
    (if (null? vars)
        body
        (make-let (car vars)
                  (car exps)
                  (make-inner-let (cdr vars) (cdr exps) body))))

  (let ((clauses (let-clauses exp))
        (body (let-body exp)))
    (let ((vars (map let-variable clauses))
          (exps (map let-expression clauses)))
      (make-inner-let vars exps body))))
; (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
;;;;;;;;;;;;;;

; exercise-4.8
(define (namedlet-var exp) (cadr exp))
(define (namedlet-bindings exp) (caddr exp))
(define (namedlet-body exp) (cdddr exp))
(define (bindings-var b) (car b))
(define (bindings-exp b) (cadr b))

(define (namedlet->combination exp)
  (let ((var (namedlet-var exp))
        (bindings (namedlet-bindings exp))
        (body (namedlet-body exp)))
    (let ((vars (map bindings-var bindings))
          (exps (map bindings-exp bindings)))
      (list
       'begin
       (append '(define)
             (list (append (list var) vars))
             body)
       (append (list var) exps)))))

(define (let->combination exp)
  (if (pair? (cadr exp))
      (simplelet->combination exp)
      (namedlet->combination exp)))

;(define (fib n)
;    (let fib-iter ((a 1)
;                  (b 0)
;                  (count n))
;     (if (= count 0)
;         b
;         (fib-iter (+ a b) a (- count 1)))))
;;;;;;;;;;;;;;

; TODO exercise-4.9
; do/for/while/until

(define (do? exp) (tagged-list? exp 'do))
(define (do-bindings exp) (cadr exp))
(define (do-clause exp) (caddr exp))
(define (do-body exp) (cdddr exp))

(define (binding-variable binding) (car binding))
(define (binding-init binding) (cadr binding))
(define (binding-step binding) (caddr binding))

(define (clause-test clause) (car clause))
(define (clause-expression clause) (cdr clause))

(define (do->lambda exp)
  (define loop-name 'iter)
  (define (make-consequent body steps)
    (append body
            (list (append (list loop-name) steps))))
  (define (make-alternative body)
    (sequence->exp body))
  
  (let ((bindings (do-bindings exp))
        (clause (do-clause exp))
        (body (do-body exp)))
    (let ((vars (map binding-variable bindings))
          (inits (map binding-init bindings))
          (steps (map binding-step bindings))
          (test (clause-test clause))
          (else-exp (clause-expression clause)))

      (list
       (list 'lambda
             '()
             (list 'define (append (list loop-name) vars)
                   (list 'if test
                         (sequence->exp (make-alternative else-exp))
                         (sequence->exp (make-consequent body steps))))
             (append (list loop-name) inits))))))

;(do ((i 0 (+ i 1)))
;  ((> i 3) (display (+ i 100)))
;  (display i))
;;;;;;;;;;;;;;

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "too many arguments supplied" vars vals)
          (error "too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable -- set!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))



(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'list list)
        (list 'null? null?)
        (list 'display display)
        (list 'assoc assoc)
        (list 'eq? eq?)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        ; exercise-4.14
        ; (list 'map map)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (my-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

; (driver-loop)

(provide (all-defined-out))
