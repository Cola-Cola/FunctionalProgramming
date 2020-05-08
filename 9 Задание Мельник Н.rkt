#lang racket
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

 (define integers (integers-starting-from 1))
 (define (printN n seq)
    (if (= n 0) 'done
        (begin 
               (display (stream-first seq))
               (newline)
               (printN (- n 1) (stream-rest seq)))))

(define (bind-first f a)
   (lambda (x) (f a x)))

 (define (f1 x y) (+ x y))
 (define g (bind-first f1 7))
 (g 5)

;Шаг 1
(define (bind-second f a)
   (lambda (x) (f x a)))

 (define (f3 x y) (- x y))
 (define g3 (bind-second f3 7))
 (g3 5)

;Задание 1
(define (add-stream s1 s2)
 (stream-cons (+ (stream-first s1) (stream-first s2)) (add-stream (stream-rest s1) (stream-rest s2))))

 (define add-str (add-stream integers integers))
 (printN 20 add-str)

;Задание умножение на число
(define (mult-stream s1 num)
    (stream-cons (* (stream-first s1) num) (mult-stream (stream-rest s1) num )))
 (define mult-str (mult-stream integers 4))
 (printN 5 mult-str)

;Задание 2
(define (mult-num-pos-stream s1 num pos)
    (cond ((= pos 0)   
    (stream-cons (* (stream-first s1) num) (mult-num-pos-stream (stream-rest s1) num 0)))
    (else (> pos 0) (stream-cons (stream-first s1) num) (mult-num-pos-stream (stream-rest s1) num (- pos 1)))))

 (define mnp-stream (mult-num-pos-stream integers 2 3))
 (printN 10 mnp-stream)

;Задание 3

(define (filter-stream pred str)
       (if(pred (stream-first str))
          (stream-cons(stream-first str)
                      (filter-stream pred (stream-rest str)))
       (filter-stream pred (stream-rest str))))

 (define evens(filter-stream even? integers))
 (define odds (filter-stream odd? integers))

(define (merge-stream s1 s2)
	(cond
	 ((null? s1) s2)
	 ((null? s2) s1)
	 ( (< (stream-first s1) (stream-first s2))
	    (stream-cons (stream-first s1) (merge-stream (stream-rest  s1) s2)))
         (else
	    (stream-cons (stream-first s2) (merge-stream (stream-rest  s2) s1)))))

 (define evod (merge-stream evens odds))
 (printN 5 evod)

;Задания 4

(define (sum-digits a)
    (if (= a 0) 0
    (+ (remainder a 10) (sum-digits (quotient a 10)))))

(sum-digits 133)

;Задание 5

(define (sum-digits-r num)
  
 	(define (tmp num sm)
          (if (= 0 num) sm
		(tmp (quotient num 10) (+ sm (remainder num 10)))))
	(tmp num 0))
(sum-digits-r 93)
