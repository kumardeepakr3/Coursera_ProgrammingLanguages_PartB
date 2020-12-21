;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
;; CHANGE (put your solutions here)
(define (racketlist->mupllist rs)
  (cond
    [(null? rs) (aunit)]
    [#t (apair (car rs) (racketlist->mupllist (cdr rs)))]
    )
  )

(define (mupllist->racketlist ms)
  (cond
    [(aunit? ms) null]
    [(apair? ms) (cons (apair-e1 ms) (mupllist->racketlist (apair-e2 ms)))]
    )
  )


;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]

        [(int? e)
          e]

        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        
        ;; CHANGE add more cases here
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL comparison done on non-number")))]

        [(closure? e)
         e]

        [(aunit? e)
         e]
        
        [(fun? e)
         (closure env e)]

        
        [(mlet? e)
         (let ([newEnv
                (append
                 (list (cons (mlet-var e) (eval-under-env (mlet-e e) env)))
                 env)])
           (eval-under-env (mlet-body e) newEnv))]

        [(call? e)
         (let ([exp1 (eval-under-env (call-funexp e) env)]
               [exp2 (eval-under-env (call-actual e) env)])
           (if (closure? exp1)
               (let* ([func (closure-fun exp1)]
                      [funcName (fun-nameopt func)]
                      [funcFormal (fun-formal func)]
                      [funcBody (fun-body func)]
                      [closureEnv (closure-env exp1)]
                      [tempEnv (cons (cons funcFormal exp2) closureEnv)]
                      [finalEnv (if (eq? funcName #f) tempEnv (cons (cons funcName exp1) tempEnv))])
                 (eval-under-env funcBody finalEnv))
               (error "MUPL call failed because arg1 is not closure")))]

        [(apair? e)
         (let ([e1 (apair-e1 e)]
               [e2 (apair-e2 e)])
           (apair (eval-under-env e1 env) (eval-under-env e2 env)))]

        [(fst? e)
         (let ([expEval (eval-under-env (fst-e e) env)])
           (if (apair? expEval)
               (apair-e1 expEval)
               (error "MUPL expression for fst evaluated to a non-pair")))]

        [(snd? e)
         (let ([expEval (eval-under-env (snd-e e) env)])
           (if (apair? expEval)
               (apair-e2 expEval)
               (error "MUPL expression for snd evaluated to a non-pair")))]

        [(isaunit? e)
         (let ([expEval (eval-under-env (isaunit-e e) env)])
           (if (aunit? expEval) (int 1) (int 0)))]

        [#t (error (format "bad MUPL expression: ~v" e))]))





;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))





;; Problem 3

; 3.a
(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

; 3.b
(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [(let* ([headPair (car lstlst)]
                [si (car headPair)]
                [ei (cdr headPair)])
           (mlet si ei (mlet* (cdr lstlst) e2)))] ))

; 3.c
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3) ))))



;; Problem 4
;(struct fun  (nameopt formal body)
(define mupl-map
  (fun #f "curriedF"
       (fun "myRec" "mupllst"
            (ifaunit (var "mupllst")
                     (aunit)
                     (apair (call (var "curriedF") (fst (var "mupllst")))
                            (call (var "myRec") (snd (var "mupllst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
          (call (var "map") (fun #f "x" (add (var "i") (var "x")))))))



;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
