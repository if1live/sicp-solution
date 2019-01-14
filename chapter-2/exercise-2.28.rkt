(define (fringe tree)
  (define (iter items results)
    (cond ((null? items) results)
          ((pair? (car items)) 
              (append (iter (car items) results)
                      (iter (cdr items) (list))))
          (else 
              (iter (cdr items) (append results (list (car items)))))))
  (iter tree (list)))

(define (fringe2 tree)
  (define (iter items results)
    (cond ((null? items) results)
          ((not (pair? items)) (append results (list items)))
          (else (append (iter (car items) results)
                        (iter (cdr items) (list))))))
  (iter tree (list)))        

(define (fringe3 tree)
  (cond ((null? tree) '())
        ((pair? tree) (append (fringe3 (car tree)) (fringe3 (cdr tree))))
        (else (list tree))))

(define x (list (list 1 2) (list 3 4)))

(print (fringe x))
(print (fringe (list x x)))

(print (fringe2 x))
(print (fringe2 (list x x)))

(print (fringe3 x))
(print (fringe3 (list x x)))
