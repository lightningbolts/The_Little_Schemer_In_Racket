#lang racket


(define (length? lat)
  (cond
    [(null? lat) 0]
    [(+ 1 (length? (cdr lat)))]
    ))

(define (tup+ tup1 tup2)
  (cond
    [(and (null? tup1) (null? tup2) '())]
    [(and (null? tup1) (not (null? tup2)) tup2)]
    [(and (null? tup2) (not (null? tup1)) tup1)]
    [(cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]
    ))

;; somehow it does not show #f when the return is #f
(define (greater? m n)
  (cond
    [(zero? m) false]
    [(zero? n) true]
    [(greater? (sub1 m) (sub1 n))]
    ))

(define (less? m n)
  (cond
    [(zero? n) #f]
    [(zero? m) #t]
    [(less? (sub1 m) (sub1 n))]
    ))