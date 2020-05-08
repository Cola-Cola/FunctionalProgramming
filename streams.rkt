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
               
(define (facts-from n f)
(stream-cons f (facts-from (+ n 1)(* f (+ n 1)))))

(define facts (facts-from 1 1))

(define (fib-from n f)
(stream-cons f (fib-from f(+ n f))))

(define fibs (fib-from 0 1))

(define (filter-stream pred seq)
  (if (pred (stream-first seq))
  (stream-cons (stream-first seq )(filter-stream pred (stream-rest seq)))
  (filter-stream pred(stream-rest seq))))
  
(define evens(filter-stream even? integers))
 (define (not-div-3 n)
    (not (= (remainder n 3) 0)))
 (define (not-div-7 n)
    (not (= (remainder n 7) 0)))
    
(define int-not-7 (filter-stream not-div-7 integers))

(define int-not-3-7 (filter-stream not-div-3 int-not-7))

;аналог функции transform, назвать transform-stream
(define (transform-stream pred seq)
  (stream-cons (pred(stream-first seq))(transform-stream pred (stream-rest seq))))
(printN 5 (transform-stream sqr integers))
