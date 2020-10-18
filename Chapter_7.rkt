#lang racket

(define (set? lat)
  (cond
    [(null? lat) #t]
    [(member? (car lat) (cdr lat)) #f]
    [else (set? (cdr lat))]))

(define (makeset lat)
  (cond
    [(null? lat) '()]
    [(member? (car lat) (cdr lat)) (makeset (cdr lat))]
    [else (cons (car lat) (makeset (cdr lat)))]))

(define (subset? set1 set2)
  (cond
    [(null? set1) #t]
    [else (and (member? (car set1) set2) (subset? (cdr set1) set2))]))

(define (eqset? set1 set2)
    (and (subset? set1 set2) (subset? set2 set1)))

(define (intersect? set1 set2)
  (cond
    [(null? set1) #f]
    [(member? (car set1) set2) #t]
    [else (intersect? (cdr set1) set2)]))

(define (intersect set1 set2)
  (cond
    [(null? set1) '()]
    [(member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2))]
    [else (intersect (cdr set1) set2)]))

(define (union set1 set2)
  (cond
    [(null? set1) set2]
    [(member? (car set1) (set2)) (union (cdr set1) (set2))]
    [else (cons (car set1) (union (cdr set1) set2))]))

(define (intersectall l-set)
  (cond
    [(null? (cdr l-set) (car l-set))]
    [else (intersect (car l-set) (intersectall (cdr l-set)))]))

(define (a-pair? x)
  (cond
    [(atom? x) #f]
    [(null? x) #f]
    [(null? (cdr x)) #f]
    [(null? (cdr (cdr x))) #f]
    [else #f]))

(define (revrel rel)
  (cond
    [(null? rel) '()]
    [else (cons (cons (car (cdr (car rel))) (cons (car (car rel)) '()))) (revrel (cdr rel))]))

(define (member? a b)
  (cond
    [(null? b) #f]
    [(eq? a (car b)) #t]
    [else (member? a (cdr b))]))

(define (atom? x)
  (not list? x))