#lang racket

(define (rember a lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) a) (cdr lat)]
    [(cons (car lat) (rember a (cdr lat)))]
    ))

(define (multirember a lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) a) (multirember a (cdr lat))]
    [(cons (car lat) (multirember a (cdr lat)))]
    ))

(define (firsts l)
  (cond
    [(null? l) '()]
    [(cons (car (car l)) (firsts (cdr l)))]
    ))

(define (insertR new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons old (cons new lat))]
    [(cons (car lat) (insertR new old (cdr lat)))]
    ))

(define (multiinsertR new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat))))]
    [(cons (car lat) (multiinsertR new old (cdr lat)))]
    ))

(define (insertL new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new lat)]
    [(cons (car lat) (insertL new old (cdr lat)))]
    ))

(define (multiinsertL new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat))))]
    [(cons (car lat) (multiinsertL new old (cdr lat)))]
    ))

(define (subst new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new (cdr lat))]
    [(cons (car lat) (subst new old (cdr lat)))]
    ))

(define (multisubst new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new (multisubst new old (cdr lat)))]
    [(cons (car lat) (multisubst new old (cdr lat)))]
    ))

(define (subst2 new old1 old2 lat)
  (cond
    [(null? lat) '()]
    [(or (eq? (car lat) old1) (eq? (car lat) old2)) (cons new (cdr lat))]
    [(cons (car lat) (subst2 new old1 old2 (cdr lat)))]
    ))

