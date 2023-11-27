#lang racket

;OTACI (1 2 3) podle poctu move
(define (rotate-zero lst move) (if (= move 0) lst(rotate-zero ( cons (car(foldl cons '() lst)) (foldl cons '() (cdr (foldl cons '() lst) ))) (- move 1))))

;Vrati otocenou matici o col
(define (find-zero matrix index i move) (if (null? matrix) null (cons (if (= index i) (rotate-zero (car matrix) move) (car matrix))(find-zero (cdr matrix) index (+ i 1) move))))

;Vraci co je na indexu
(define (on-index lst index) (if (= index 0) (car lst) (on-index (cdr lst) (- index 1))))

;Vraci zmenenou vektory o posunuti
(define (my-change lst x index i)  (cond((null? lst) null)(else (if (= i index) (cons x (cdr lst)) (cons (car lst) (my-change (cdr lst) x index (+ i 1) ))))))

;vraci list vsech elementu na indexu
(define (on-indexes matrix index)(map (lambda (x) (on-index x index))matrix))

;vraci otocenou matici o row
(define (find-one matrix index lst)
  (cond
    ((null? matrix)null)
    (else (cons (my-change (car matrix) (car lst) index 0) (find-one (cdr matrix) index (cdr lst))))))
  
  
(define (len lst)
  (if (null? lst)
      0
      (+ 1 (len (cdr lst)))))
  
#|
(define (rotate-one lst move index)(if ( null? (cadr lst) ) null (let ((x (on-index (car lst) index)))(my-change (cadr lst) x index 0 ))))
|#

;command (1 0 1)
; 0. 1 -> sloupce 0-> radky
; 1. index
; 2. posun

(define (rotate matrix command)
  (define row-or-col (car command))
  (define index (cadr command))
  (define move-number (cadr(cdr command)))
  (define move (if (= row-or-col 1) (modulo move-number (len matrix)) (modulo move-number (len (car matrix))) ))
  (cond
    ((null? command) matrix)
    ((= row-or-col 0) (find-zero matrix index 0 move)) 
    (else (find-one matrix index (rotate-zero(on-indexes matrix index)move) ))
    )
  )

(define (rotate-mat matrix commands)
  (define com (filter (lambda (x) (= (len x) 3)) commands))
  (if (null? com) matrix (rotate-mat (rotate matrix (car com)) (cdr com)))
  )