#lang racket

(define (lat? l)
  (cond
    [(null? l) #t]
    [(atom? (car l)) (lat? (cdr l))]
    [else #f]
    ))

(define (atom? a)
  (and (not (null? a)) (not (pair? a))))

(define (lat2? l)
  (if (null? l)
      #t
      (if (atom? (car l))
          (lat? (cdr l))
          #f)))