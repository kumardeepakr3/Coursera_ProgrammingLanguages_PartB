
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
; Problem 1
(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null)
)

; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix) ) xs)
)

; Problem 3
(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]    
  )
)

; Problem 4
(define (stream-for-n-steps s n)
    (if (= n 0)
        null
        (let ([strElem (s)])
          (cons (car strElem) (stream-for-n-steps (cdr strElem) (- n 1)))
        )
     )
)

; Problem 5
(define funny-number-stream
  (letrec ([negateIfMult5 (lambda (t) (if (= (remainder t 5) 0) (- 0 t) t))]
           [f (lambda (x) (cons (negateIfMult5 x) (lambda () (f (+ x 1)))) )]
          )
    (lambda () (f 1)))
)

; Problem 6
(define dan-then-dog
  (letrec ([dan "dan.jpg"]
           [dog "dog.jpg"]
           [dog-then-dan (lambda () (cons dog dan-then-dog))]
           )
    (lambda () (cons dan dog-then-dan))
  )
)

; Problem 7
(define (stream-add-zero s)
  (letrec(
          [strElem (s)]
          )
    (lambda () (cons (cons 0 (car strElem)) (stream-add-zero (cdr strElem)) ))
  )
)

; Problem 8
(define (cycle-lists xs ys)
  (letrec ([getNthPair (lambda (n) (cons (list-nth-mod xs n) (list-nth-mod ys n)))]
           [getPairedStreamFromN (lambda (n) (cons (getNthPair n) (lambda () (getPairedStreamFromN (+ n 1))) ))]
           )
    (lambda () (getPairedStreamFromN 0))
  )
)

; Problem 9
(define (vector-assoc v vec)
  (letrec ([veclen (vector-length vec)]
           [searchInVectorAtIndex
            (lambda (n)
              (if (= n veclen)
                  #f
                (letrec ([elemAtI (vector-ref vec n)])
                  (if (pair? elemAtI)
                       (if (equal? v (car elemAtI)) elemAtI (searchInVectorAtIndex (+ n 1)))
                       (searchInVectorAtIndex (+ n 1))
                  )
                )
              )
            )
           ])
    (searchInVectorAtIndex 0)
  )
)

; Problem 10
(define (cached-assoc xs n)
  (letrec ([posToUpdate 0]
           [localCache (make-vector n #f)]
           [localAssoc
            (lambda (v)
              (letrec ([cachedResult (vector-assoc v localCache)])
                (if cachedResult
                    cachedResult
                    (letrec ([res (assoc v xs)])
                      (begin
                        (vector-set! localCache posToUpdate res)
                        (set! posToUpdate (remainder (+ posToUpdate 1) n))
                        res)
                      )
                )
              )
            )]
           )
    
    localAssoc)
)




