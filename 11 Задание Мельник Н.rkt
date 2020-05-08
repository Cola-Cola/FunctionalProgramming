#lang racket
(define (get-val tree)
    (car tree))

(define (get-left tree)
    (cadr tree))

(define (get-right tree)
    (caddr tree))

(define (make-tree val left right)
    (list val left right))

(define (add-val-tree val tree)
    (cond ((null? tree) (make-tree val '() '()))
          ((> val (get-val tree)) (make-tree (get-val tree) (get-left tree) (add-val-tree val (get-right tree))))
          ((< val (get-val tree)) (make-tree (get-val tree) (add-val-tree val (get-left tree)) (get-right tree) ))
          ((= val (get-val tree)) tree)))

;Задание 1
(define (add-list-tree lst tree)
      (if (null? lst) tree
          (add-list-tree (cdr lst) (add-val-tree (car lst) tree))))

 (add-val-tree 5 '())
 (define aa (add-val-tree 5 '()))
(add-list-tree '(10 4) aa)
(define bb (add-list-tree '(5 3 8) '()))

(define cc (add-list-tree '(9 4 15 5 1 7 3 10 18 12) '()))

;Задание 2

(define (size-tree tree)
    (if (null? tree) 0
        (+ 1 (size-tree(get-left tree)) (size-tree (get-right tree)))))
(size-tree cc)

;Задание 3

(define (depth-tree tree)
    (if (null? tree) 0
        (+ 1 (max (depth-tree(get-left tree)) (depth-tree(get-right tree))))))
(depth-tree cc)

;Задание 4

(define (append l1 l2)
  (if (null? l1)
      l2
    (cons (car l1) (append (cdr l1) l2))))

 (define (lr-travel-tree tree)
       (if (null? tree) '()
           (append (lr-travel-tree (get-left tree)) (cons (get-val tree) ( lr-travel-tree (get-right tree))))))
(lr-travel-tree cc)
(define (rl-travel-tree tree)
       (if (null? tree) '()
           (append (rl-travel-tree (get-right tree)) (cons (get-val tree) ( rl-travel-tree (get-left tree))))))
(rl-travel-tree cc)
;Задание 4 Иерархически
(define (hlr-travel-tree tree)
       (if (null? tree) '()
           (append (cons (get-val tree) ( hlr-travel-tree (get-left tree))) (hlr-travel-tree (get-right tree)))))
(hlr-travel-tree cc)



