#lang racket
(define (count-combinations lst n)
    (cond
      ((= n 0) 1)   
      ((null? lst) 0) 
      (else
       (+ (count-combinations (cdr lst) (- n (car lst))) 
          (count-combinations (cdr lst) n)))))


(define (comb lst n)
  (count-combinations lst n))