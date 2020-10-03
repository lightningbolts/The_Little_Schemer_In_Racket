#lang racket

(define (length lat)
  (cond
    [(null? lat) 0]
    [(add1 (length (cdr lat)))]
    ))

(define (pick n lat)
  (cond
    [(zero? (sub1 n) (car lat))]
    [(pick (sub1 n) (cdr lat))]
    ))

(define (rempick n lat)
  (cond
    [(zero? (sub1 n (cdr lat)))]
    [(cons (car lat) (rempick (sub1 n) (cdr lat)))]
    ))

(define (no-nums lat)
  (cond
    [(null? lat) '()]
    [(number? (car lat))]
    [(no-nums (cdr lat))]
    [(cons (car lat) (no-nums (cdr lat)))]))

(define (all-nums lat)
  (cond
    [(null? lat) '()]
    [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
    [(all-nums (cdr lat))]))

(define (eqan? a1 a2)
  (or (eq? a1 a2) (= a1 a2)))

(define (occur a lat)
  (cond
    [(eq? (car lat) a)]
    [(add1 (occur a (cdr lat)))]
    [(occur a (cdr lat))]))

(define (one? n)
  (= n 1))

(define (rempick1 n lat)
  (cond
    [(one? n) (cdr lat)]
    [(cons (car lat) (rempick1 (sub1 n) (cdr lat)))]))


    