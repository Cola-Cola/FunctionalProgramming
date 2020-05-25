#lang racket
;Задание 1
(define (negate pred)
	(lambda (x) (not (pred x))))

 (define ff (negate even?))

(ff 2)

(ff 3)

;Задание 2

(define (filter p lst)
    (if (null? lst) null	
        (if (p (car lst))
            (cons (car lst)
            (filter p (cdr lst)))
            (filter p (cdr lst)))))

 (define (append l1 l2)
  (if (null? l1)
      l2
    (cons (car l1) (append (cdr l1) l2))))

 (define a '(1 2 3 4 5 6 7))

(define (reorder pred lst)
    (append (filter pred lst) (filter (negate pred) lst)))

(reorder even? a)

(reorder odd? a)

;Задание 3



(define (after-val val lst)
    (cond
      ((null? lst) '())
      ((= (car lst) val) (cdr lst))
      (else
       (after-val val (cdr lst)))))

(after-val 3 a)

;Задание 4

(define (before-val val lst)
    (cond
      ((null? lst) '())
      ((= (car lst) val) '())
      (else
       (cons (car lst) (before-val val (cdr lst))))))

(before-val 4 a)


;Задание 5

(define (pred-val pred value)
	(if (pred value) pred 
		(negate pred)))

(define (filter-by-first pred lst)
    (if (null? lst) '()
        (filter (pred-val pred (car lst)) lst)))

(define b '(2 3 4 5 6 7))

(filter-by-first even? a)
(filter-by-first odd? b)