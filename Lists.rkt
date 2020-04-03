#lang scheme
; Задание 1
(define (padd p1 p2)
    (cond ((null? p1) p2)
          ((null? p2) p1)
      ( else   (cons (+ (car p1) (car p2)) (padd (cdr p1) (cdr p2))))))

; Задание 2
(define (pmulc p c)
   (if (null? p)
        '()
        (cons (* c (car p)) (pmulc (cdr p) c))))

; Задание 3
(define (shift p m)
    (if (= m 0) p
        (cons 0 (shift p (- m 1)))))

(define (pmulm p m c)
    (pmulc (shift p m) c))

; Задание 4
(define (power x n)
    (if (= n 0)
        1
        (* x (power x (- n 1)))))

(define (calcp p x)
  (define (tmp p x val m)
    (if (null? p) val
        (tmp (cdr p) x (+ val (*(car p) (power x m))) (+ m 1))))
        (tmp p x 0 0))

; Домашнее задание
(define( up-degree p n)
    (append (zeros n) p))

(define (zeros n)
    (if (< n 1)
        '()
        (cons 0 (zeros (- n 1)))))

(define (pmulp p1 p2)
    (if (null? p2)
        '(0)
        (padd (pmulc p1 (car p2))
             (pmulp (up-degree p1 1) (cdr p2)))))