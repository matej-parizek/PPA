#lang racket

(define (my-append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (my-append (cdr lst1) lst2))
  )
)
(define (my-reverse lst)
  (if (null? lst)
      null
      (if (not(list? lst))
          lst
      (my-append (my-reverse (cdr lst)) (cons (car lst) '() ))
      ))
  )
(define (reverse-tree bst)
  (if (null? bst)
      null
      (if(not(list? bst))
         bst
      (cons (reverse-tree (car bst)) (reverse-tree (my-reverse(cdr bst)) ))) )
  )