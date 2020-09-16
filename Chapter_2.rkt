#lang racket

(define (rember a lat)
  (cond
    [(null? lat) (quote ())]
    [(eq? a (car lat)) (cdr lat)]
    [(cons (car lat) (rember a (cdr lat)))]
    ))

(rember 'five-star-duck '(chicken-soup fried-potatoes five-star-duck bell-pepper orange-chicken chow-mein fried-pea-rice))
