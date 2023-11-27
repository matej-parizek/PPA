#lang racket

(define (my-fill key alist)
  (let ((result (filter (lambda (x) (= (car x) key))alist)))
    (if (null? result) #f
        (car result))))

(define (my-cnf keys alist)
 (let ((result (filter ( lambda (y) ( and y #t))
   (map( lambda (x)
         (cadr(my-fill x alist)))keys))))
   (if (null? result) #f #t)
   ))

(define (cnf-value form values)
  (if (null? 
(filter (lambda (y) (equal?  #f y) )
        (map (lambda (x) (my-cnf x values))form))
  )#t #f))
