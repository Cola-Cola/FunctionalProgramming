#lang racket
;Задание 1
(define (deriv f dx)
    (lambda (x) (/ (- (f (+ x dx))(f x)) dx)))

 (define de (deriv exp 1e-6))

 (de 1)

(define (deriv1 f dx)
    (lambda (x) (/ (- (f (+ x dx))(f (- x dx))) (* 2 dx))))

 (define de1 (deriv1 exp 1e-6))

 (de1 1)

;Задание 2


(define (make-list num)
    (if (= num 0) null
        (cons (remainder num 10) (make-list (quotient num 10)))))

(define (make-num lst)
    (if (null? lst) 0
        (+ (car lst) (* 10 (make-num (cdr lst))))))


(define (append l1 l2)
  (if (null? l1)
      l2
    (cons (car l1) (append (cdr l1) l2))))

(define (reverse lst)
    (if (null? lst) null
    (append (reverse (cdr lst)) (cons (car lst) null))))

(define (invert num)
    (make-num (reverse (make-list num))))

(invert 543)

;Задание 3

;a)

(define compl (cons 'dec (cons 3 2 )))
(define comp2 (cons 'polar (cons 1 (/ pi 2))))

(define (get-x compl)
    (if (eq? (car compl) 'dec)
    (car (cdr compl))
    (* (car (cdr compl)) (cos (cdr (cdr compl))))))

(get-x compl)
(get-x comp2)

(define (get-y compl)
    (if (eq? (car compl) 'dec)
    (cdr (cdr compl))
    (* (car (cdr compl)) (sin (cdr (cdr compl))))))

(get-y compl)
(get-y comp2)

;б)

(define (get-r compl)
    (sqrt (+ (get-x compl) (get-y compl))))

(get-r compl)

(define (get-angle compl)
    (atan (get-x compl) (get-y compl)))

(get-angle compl)

;в)

(define (make-compl-dec x y)
	(cons (cons 'dec '()) (cons x y)))
(define (make-compl-polar r a)
	(cons (cons 'pol '()) (cons r a)))

