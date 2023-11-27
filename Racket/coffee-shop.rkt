#lang racket

(define (my-append lst1 lst2)
  (if (null? lst1)
      lst2
      (if (not(list? lst1))
          (cons lst1 lst2)
          (cons (car lst1) (my-append (cdr lst1) lst2)))
  )
)

(define (cyklus f m)
  (if (null? f)
      0
      (if(null? m)
         0
      (if (equal? m (car f))
      (+ (cyklus (cdr f) m) 1)
      (+ (cyklus (cdr f) m) 0)
      ))
  )
  )
(define (my-max lst)
  ( if( null? lst)
      0
      (if (< (car lst) (my-max(cdr lst)))
         (my-max(cdr lst))
         (car lst)
         )
  ))
(define (coffee-shop-lst times)
  (if(null? times)
     null
    (my-append (+ (cyklus (cdr times) (car times)) 1)  (coffee-shop-lst (cdr times)))
           
  )
  )

(define (coffee-shop times) (my-max(coffee-shop-lst times)))


#|#lang racket

(define (dfs paths from to visited)
  (define next-path (filter (lambda (x) (= (car x) from)) paths))

  
  (cond
    ((= from to) #t)
    ((null? paths) #f)
    (else(if(null? (filter( lambda (x) (= x from)) visited))
          (map (lambda (x)
                
                  (if (= (cadr x) to) #t (dfs paths (cadr x) to (append visited x)))

                 )
                   next-path)
          #f))
))

(define (cities-path? paths from to)
  (dfs paths from to '())
  )

  |#