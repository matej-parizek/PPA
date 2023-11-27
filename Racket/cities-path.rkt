#lang racket

(define (contains-number? lst x)
  (cond
    ((null? lst) #f)                
    ((= (car lst) x) #t)          
    (else (contains-number? (cdr lst) x))))


(define (bfs paths from to visited next)
  (define next-path (filter (lambda (x) (and (= (car x) from) (not(contains-number? visited (cadr x))))) paths))
  (define next-states (foldr cons (map (lambda (x) (cadr x))next-path) next))

  (cond
    ((null? to) #f)
    ((null? from) #f)
    ((= from to) #t)
    ((null? paths) #f)
    ((and (null? next-path) (null? next)) #f)
    ((null? next-states )#f)
    (else (bfs paths (car next-states) to (cons from visited) (cdr next-states)))
  ))
  
(define (cities-path? paths from to)
  (bfs paths from to '() '())
  )
