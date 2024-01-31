#lang racket
;Cvičení 1
 ;Foldl
(define (my-foldl foo init lst)
  (if (null? lst)
      init
      (my-foldl foo (foo init (car lst)) (cdr lst))))
 ;Foldr
(define (my-foldr foo init lst)
  (if (null? lst)
      init
      (foo (my-foldr foo init (cdr lst)) (car lst))))
;------------------------------------------------------------------------------------

;Cvičšní 2
 ;Map
(define (my-map foo lst)
  (if (null? lst)
      null
      (cons (foo(car lst)) (my-map foo (cdr lst)))))
 ;Filter
(define (my-filter foo lst)
  (if (null? lst)
      null
      (if (foo (car lst))
          (cons (car lst) (my-filter foo (cdr lst)))
          (my-filter foo (cdr lst)))))
 ;Append
(define (my-append lst val)
  (if (null? lst)
      (cons val null)
      (foldr cons (cons val null) lst)))
 ;Reverse
(define (my-reverse lst)
  (foldl cons null lst))
;------------------------------------------------------------------------------------

;Cvičení 3
; TreeSort
;------------------------------------------------------------------------------------

;Cvičení 4
;My-zip-map
(define (my-zip-map lst1 lst2)
  (define foo (lambda (x y) (cons x (cons y null))))
  (cond
    ((null? lst1) null)
    ((null? lst2) null)
    (else (cons (foo (car lst1) (car lst2)) (my-zip-map (cdr lst1) (cdr lst2))))))
;------------------------------------------------------------------------------------

;Cvičení 5
 ;MERGESORT - chybí

 ;MEAN
(define (avg lst)
  (let ((sum (foldr + 0 lst))
        (count ( foldr (lambda (x y) (+ (- x (- x 1)) y)) 0 lst)))
    (if (null? lst)
        0
        (/ sum count ))))
;------------------------------------------------------------------------------------

;Cvičení 6
 ;middle element
(define (my-select-middle-element lst)
  (define (foo p1 p2)
    (cond
      ((null? p2) (car p1))
      ((null? (cdr p2)) (car p1))
      (else (foo (cdr p1) (cddr p2)))))
  (if (null? lst)
      null
      (foo lst lst)))
 ;filter prvky > n
(define ( my-filter-gt lst n)(my-filter (lambda (x) (> x n)) lst))
 ;filer prvky < n
(define ( my-filter-lt lst n)(my-filter (lambda (x) (< x n)) lst))
 ;filter prvky = n
(define ( my-filter-eq lst n)(my-filter (lambda (x) (= x n)) lst))
;------------------------------------------------------------------------------------

;Cvičení 7
(define (my-sieve n)
  (my-sieve-aux (cddr (range (+ n 1))))) ; initialize with list 2...n

(define (my-sieve-aux sieve)
  (if (null? sieve)
      null
      (cons (car sieve)                                                                         ; move first element (surely a prime) into the result
            (my-sieve-aux (my-filter (lambda (e) (not (= 0 (modulo e (car sieve))))) sieve))))) ; erase all elements that are not divisible by first element of the list

(provide (all-defined-out))
