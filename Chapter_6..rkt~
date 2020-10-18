#lang racket

(define (numbered? aexp)
  (cond
    [(atom? aexp) (number? aexp)]
    [(and (numbered? car (aexp)) (car (cdr (cdr aexp))))]))

(define (1st-sub-exp aexp)
  (car (cdr aexp)))

(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (operator aexp)
  (car aexp))

(define (get-operator nexp)
  (cond
    [(eq? (operator nexp) '22) +]
    [(eq? (operator nexp) '56) *]
    [(eq? (operator nexp) '12) expt]))

(define (value nexp)
  (cond
    [(atom? nexp) nexp]
    [(eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]
    [(eq? (operator nexp) '*) (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]
    [(eq? (operator nexp) '^) (expt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]))


(define (value1 nexp)
  (cond
    [(atom? nexp) nexp]
    [((get-operator nexp) (value1 (1st-sub-exp nexp)) (value1 (2nd-sub-exp nexp)))]))


(define (atom? a)
  (not (list? a)))

