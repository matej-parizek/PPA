#lang racket
(require compatibility/defmacro)

;Cvičení 2
;If null macro
(define-macro (ifNull value true_branch false_branch) `(if (null? ,value) ,true_branch ,false_branch))
 ;test Macro
(define (test-macro)(ifNull '() #t #f))
;------------------------------------------------------------------------------------

;Cvičení 3
;let
(define-macro (my-let bind-pair body)
  `(
    (lambda( ,(car bind-pair)) ,body) ,(cadr bind-pair)))

; let aby dovolovalo libovolný počet bind-pair (podobně jako let v Racketu)
(define-macro (my-let-racket bind-pair body)
  `(
    (lambda (,@(map car bind-pair )) ,body)
    ,@(map cadr bind-pair)))
;let let*

(define-macro (my-let3 bind-pair body)
  (if (null? bind-pair)
      body
      `((lambda ( ,(car bind-pair)) (my-let3 ,(cdr bind-pair ) ,body)) ,(cadar bind-pair))))
(my-let3 [(foo 2) (bar (+ foo 1))] (+ foo bar))
;------------------------------------------------------------------------------------

; Cvičení 4

(define-macro (debug-print body)
  (let ((tmp (gensym)))
         `(let ((,tmp ,body))
          (println(format "!! debuger: ~a -> ~a" ',body ,tmp))
          ,tmp)))
;------------------------------------------------------------------------------------

; Cvičení 5
(define-macro (assert-true body)
  `(if ,body (void) (error (format "Assertion (assert-true ~a) failed" ',body)) ))
#|(define-macro (assert-true body)
  `(unless (error (format "Assertion ~a failed" ',body)) ))|#
(define-macro (assert-equal body1 body2)
  `(unless(equal? ,body1 ,body2) (error (format "Assertion (assert-equal ~a ~a) failed" ',body1 ',body2))))
;------------------------------------------------------------------------------------

;Cvičení 6
 ;S použitím pomocných funkcí
  ;(my-loop1 lb ub body), které vykoná kód body (ub - lb)-krát
(define (my-while1 iter ub foo)
  (if ( = iter ub)
       (void)
       (begin
         (foo)
         ( my-while1 (+ iter 1) ub foo))))

(define-macro (my-loop1 lb ub body)
  `(my-while  ,lb ,ub (lambda () ,body)))

  ;(my-loop2 lb ub body), které vykoná kód body (ub - lb)-krát a zároveň všechny volné výskyty n v body nahradí iterační proměnnou z funkce my-while2
(define (my-while2 iter ub foo)
  (if ( = iter ub)
       (void)
       (begin
         (foo iter)
         ( my-while2 (+ iter 1) ub foo))))

(define-macro (my-loop2 lb ub body)
  `(my-while2 ,lb ,ub (lambda (n) ,body)))

  ;(my-loop3 varname lb ub body), které vykoná kód body (ub - lb)-krát a zároveň všechny volné výskyty varname v body nahradí iterační proměnnou z funkce my-while2
(define-macro (my-loop3 lb ub varname body)
  `(my-while2 ,lb ,ub (lambda (,varname) ,body)))

;Jen pomocí macer
 ;(my-loop1)
(define-macro (my-loop-1 iter ub foo)
  (let ((f (gensym)))
    `(letrec ((,f (lambda (i u func)
                    (if (= i u)
                        (void)
                        (begin
                          (func)
                          (,f (+ i 1) u func))))))
       (,f ,iter ,ub ,foo))))
 ; (my-loop2)
(define-macro (my-loop-2 iter ub foo)
  (let ((func (gensym)))
     `(letrec ((,func (lambda (i u f)
                       (if (= i u)
                           (void)
                           (begin
                             ( f i)
                             (,func (+ i 1) u f))))))
       (,func ,iter ,ub (lambda (n) ,foo)))))
                            
;(my-loop-2 0 7   (println (format "loop2 ~a" n)))
(provide (all-defined-out))

;(my-loop3)

(define-macro (my-loop-3 lb ub varname body)
  (let ((func (gensym)))
    `(letrec ((,func (lambda (i)
                       (if( = i ,ub)
                          (void)
                          (begin
                            ((lambda (,varname) ,body) i)
                            (,func (+ i 1)))))))
                     (,func ,lb))))

          