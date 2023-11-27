#lang racket

(define (decorate f g)
  (lambda (x)
    (g (f x))))