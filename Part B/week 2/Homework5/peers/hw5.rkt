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
(define (racketlist->mupllist rlist)
  (cond [(null? rlist) (aunit)]
        [#t (apair (car rlist) (racketlist->mupllist (cdr rlist)))]))

(define (mupllist->racketlist mlist)
  (cond [(aunit? mlist) (list)]
        [#t (append (list (apair-e1 mlist)) (mupllist->racketlist (apair-e2 mlist)))]))

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
;; CHANGE add more cases here
(define (eval-under-env e env)
  (cond [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(var? e) 
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)  ;;evalue this only when necessary
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (eval-under-env (mlet-body e)
                         (append (list (cons (mlet-var e) (eval-under-env (mlet-e e) env))) env))] ;!!!put local binding ahead in the list
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [(call? e)
         (let* ([f (eval-under-env (call-funexp e) env)]
                [argv (eval-under-env (call-actual e) env)])
           (if (closure? f)
               (let* ([fname (fun-nameopt (closure-fun f))]
                      [farg (fun-formal (closure-fun f))]
                      [fbody (fun-body (closure-fun f))]
                      [bind (if (equal? fname #f)
                               (list (cons farg argv))
                               (append (list (cons farg argv)) (list (cons fname f))))])
                 (eval-under-env fbody (append bind (closure-env f))))
               (error "MUPL call applied to non-function")))]  
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst))
            (cdr (car lstlst))
            (mlet* (cdr lstlst) e2))))
      
(define (ifeq e1 e2 e3 e4) (ifgreater e1 e2 e4 (ifgreater e2 e1 e4 e3)))

;; Problem 4

;; for testing: simpler functions
;(define fib
;(fun "fib" "n"
;       (ifeq (var "n") (int 1)
;             (int 1)
;             (add (var "n") (call (var "fib") (add (var "n") (int -1)))))))
;(define addxy
;  (fun "addxy" "x"
;       (fun "addy" "y"
;            (add (var "x") (var "y")))))
;(define callf
;  (fun "callf" "f"
;       (fun "callff" "param"
;            (call (var "f") (var "param")))))

(define mupl-map
  (fun "mupl-map" "f"
       (fun "mupl-map-f" "mlst"
            (ifaunit (var "mlst")
                     (aunit)
                     (apair (call (var "f") (fst (var "mlst"))) (call (var "mupl-map-f") (snd (var "mlst"))))))))
              
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "mapAddN" "n"
             (mlet "addN" (fun #f "x" (add (var "x") (var "n")))
                   (call (var "map") (var "addN"))))))


;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (find-free-vars e)
  (cond [(var? e) (set (var-string e))]
        [(int? e) (set)]
        [(closure? e) (set)]
        [(add? e) (set-union (find-free-vars (add-e1 e))
                             (find-free-vars (add-e2 e)))]
        [(ifgreater? e) (set-union (find-free-vars (ifgreater-e1 e))
                                   (find-free-vars (ifgreater-e2 e))
                                   (find-free-vars (ifgreater-e3 e))
                                   (find-free-vars (ifgreater-e4 e)))]
        [(mlet? e) (set-union (find-free-vars (mlet-e e))
                              (set-remove (find-free-vars (mlet-body e))
                                          (mlet-var e)))]
        [(apair? e) (set-union (find-free-vars (apair-e1 e))
                               (find-free-vars (apair-e2 e)))]
        [(fst? e) (find-free-vars (fst-e e))]
        [(snd? e) (find-free-vars (snd-e e))]
        [(aunit? e) (set)]
        [(isaunit? e) (find-free-vars (isaunit-e e))]
        [(fun? e) (set-remove (set-remove (find-free-vars (fun-body e))
                                          (fun-nameopt e))
                              (fun-formal e))]
        [(call? e) (set-union (find-free-vars (call-funexp e))
                              (find-free-vars (call-actual e)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))
        
(define (compute-free-vars e)
  (cond [(var? e) e]
        [(int? e) e]
        [(closure? e) (compute-free-vars (closure-fun e))]
        [(add? e) (add (compute-free-vars (add-e1 e))
                       (compute-free-vars (add-e2 e)))]
        [(ifgreater? e) (ifgreater (compute-free-vars (ifgreater-e1 e))
                                   (compute-free-vars (ifgreater-e2 e))
                                   (compute-free-vars (ifgreater-e3 e))
                                   (compute-free-vars (ifgreater-e4 e)))]
        [(mlet? e) (mlet (mlet-var e)
                         (compute-free-vars (mlet-e e))
                         (compute-free-vars (mlet-body e)))]
        [(apair? e) (apair (compute-free-vars (apair-e1 e))
                           (compute-free-vars (apair-e2 e)))]
        [(fst? e) (fst (compute-free-vars (fst-e e)))]
        [(snd? e) (snd (compute-free-vars (snd-e e)))]
        [(aunit? e) (aunit)]
        [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
        [(call? e) (call (compute-free-vars (call-funexp e))
                         (compute-free-vars (call-actual e)))]
        [(fun? e) (fun-challenge (fun-nameopt e)
                                 (fun-formal e)
                                 (compute-free-vars (fun-body e))
                                 (find-free-vars e))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (create-free-var-env fvars env)
  (if (null? fvars)
      (list)
      (append (list (cons (car fvars) (envlookup env (car fvars))))
              (create-free-var-env (list-tail fvars 1) env))))

(define (eval-under-env-c e env)
  (cond [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(apair? e)
         (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
        [(var? e) 
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
                (int (+ (int-num v1) 
                        (int-num v2)))
                (error "MUPL addition applied to non-number")))]
         [(fun-challenge? e)
          (closure (create-free-var-env (set->list (fun-challenge-freevars e)) env)
                   (fun (fun-challenge-nameopt e) (fun-challenge-formal e) (fun-challenge-body e)))]
         [(ifgreater? e)
          (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
                [v2 (eval-under-env-c (ifgreater-e2 e) env)])
            (if (and (int? v1) (int? v2))
                (if (> (int-num v1) (int-num v2))
                    (eval-under-env-c (ifgreater-e3 e) env)  ;;evalue this only when neessary
                    (eval-under-env-c (ifgreater-e4 e) env))
                (error "MUPL ifgreater applied to non-number")))]
         [(mlet? e)
          (eval-under-env-c (mlet-body e)
                            (append (list (cons (mlet-var e) (eval-under-env-c (mlet-e e) env))) env))] ;!!!put local binding ahead in the list
         [(fst? e)
          (let ([v (eval-under-env-c (fst-e e) env)])
            (if (apair? v)
                (apair-e1 v)
                (error "MUPL fst applied to non-pair")))]
         [(snd? e)
          (let ([v (eval-under-env-c (snd-e e) env)])
            (if (apair? v)
                (apair-e2 v)
                (error "MUPL snd applied to non-pair")))]
         [(isaunit? e)
          (if (aunit? (eval-under-env-c (isaunit-e e) env)) (int 1) (int 0))]
         [(call? e)
          (let* ([f (eval-under-env-c (call-funexp e) env)]
                 [argv (eval-under-env-c (call-actual e) env)])
            (if (closure? f)
                (let* ([fname (fun-nameopt (closure-fun f))]
                       [farg (fun-formal (closure-fun f))]
                       [fbody (fun-body (closure-fun f))]
                       [bind (if (equal? fname #f)
                                 (list (cons farg argv))
                                 (append (list (cons farg argv)) (list (cons fname f))))])
                  (eval-under-env-c fbody (append bind (closure-env f))))
                (error "MUPL call applied to non-function")))]
         [#t (error (format "bad MUPL expression: ~v" e))]))
   
;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

