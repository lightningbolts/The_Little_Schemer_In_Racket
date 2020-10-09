#lang racket

(define (rember* a l)
  (cond
    [(null? l) '()]
    [(not (list? (car l))) (cond
                       [(eq? a (car l)) (rember* a (cdr l))]
                       [(cons (car l) (rember* a (cdr l)))])]
    [(cons (rember* a (car l)) (rember* a (cdr l)))]))

(define (insertR* new old lat)
  (cond
    [(null? lat) '()]
    [(list? (car lat)) (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))]
    [(cond
       [(eq? old (car lat)) (cons old (cons new (insertR* new old (cdr lat))))]
       [(cons (car lat) (insertR* new old (cdr lat)))])]))

(define (occur* a l)
  (cond
    [(null? l) 0]
    [(atom? (car l))
     (cond
       [(eq? (car l) a) (add1 (occur* a (cdr l)))]
       [(occur* a (cdr l))]
       )]
    [(+ (occur* a (car l)) (occur* a (cdr l)))]))

(define (subst* new old l)
  (cond
    [(null? l) '()]
    [(not (list? (car l)))
     (cond
       [(eq? (car l) old)]
       [(cons new (subst* new old (cdr l)))]
       [(cons (car l) (subst* new old (cdr l)))])]
    [(cons (subst* new old (car l)) (subst* new old (cdr l)))]))

(define (member* a l)
  (cond
    [(null? l) #f]
    [(not (list? (car l)))]
    [(or (member* a (car l)) (member* a (cdr l)))]))

(define (leftmost l)
  (cond
    [(not (list? (car l) (car l)))]
    [(leftmost (car l))]))

(define (eqlist? l1 l2)
  (cond
    [(and (null? l1) (null? l2)) #t]
    [(or (null? l1) (null? l2)) #f]
    [(and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]))

(define (equal? s1 s2)
  (cond
    [(and (atom? s1) (atom? s2))]
    [(eqan? s1 s2)]
    [(atom? s1) #f]
    [(atom? s2) #f]
    [(eqlist? s1 s2)]))

(define (atom? a)
  (not (list? a)))

(define (eqan? a b)
  (cond
    [(and (number? a) (number? b)) (= a b)]
    [(or (number? a) (number? b)) #f]
    [(eq? a b)]))


                     
    