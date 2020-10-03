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


                     
    