#lang racket
;сложение - умножение
(define (sqr x) (* x x))

(define (power x n)
    (cond 
	((= n 0) 1)
	((even? n) (sqr (power x (quotient n 2))))
	(else (* x (power x (- n 1))))))

(define (twice x) (+ x x))

(define (mult m n)
    (cond
      ((= n 1) m)
      ((even? n) (twice (mult m (quotient n 2))))
      (else (+ m (mult m (- n 1))))))

(mult 2 4)


;Ханойские башни

(define (hanoy n s t m )
  (if (= n 2)  (list (list s m) (list s t) (list m t))
	(append (hanoy (- n 1) s m t) (cons (list s t) (hanoy (- n 1) m t s)))))

 (hanoy 3 1 3 2)