#lang racket

(define s "Hello");

(+ 3 3);

(define x 3);
(define y (+ x 2));

(define cube1
  (lambda (x)
    (* x (* x x))));

(define cube2
  (lambda (x)
    (* x x x)));

(define (cube3 x)
  (* x x x));

(define (pow1 x y)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))));

; null - empty list, cons, car - head, cdr -tail, null? - check for empty, list - list constructor

(define mylist (list 1 2 3 4));

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))));

(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))));

(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (my-map f (cdr xs)))));


(define foo (my-map (lambda (x) (+ x 1)) (list 1 2 3 4)));

(define ones (lambda () (cons 1 ones)))

; A term is either
;  - An atom, e.g. #t, #f, 34, "hi", null, 4.0, x
;  - A special form e.g. define, lambda, if
;  - A sequence of terms in parens: (t1 t2 ... tn)
;         - If t1 is a special form, semantics of sequence is special
;         - Else it's a function call


(define (sum99 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum99 (cdr xs)))]
        [(list? (car xs)) (+ (sum99 (car xs)) (sum99 (cdr xs)))]
        [(#t (sum99 (cdr xs)))]))

; anything other than #f is considered as true

; (let ([x1 e1] [x2 e2] [x3 e3]) Body)






