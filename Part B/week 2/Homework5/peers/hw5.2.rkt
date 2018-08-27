;; Programming Languages, Homework 5


;;;APOLOGIES ABOUT ALL THE COMMENTS. I HAD FAR MORE BUT THIS DID MY HEAD IN
;;;AND I NEEDED THEM AT ALL TIMES WHEN I WAS CHECKING/RE-RUNNING TO
;;;HAVE ANY SORT OF CLUE.

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
(struct mmmlet (var e body) #:transparent)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0
;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 
;; Problem 1
(define (racketlist->mupllist rls)
  (cond [(empty? rls) (aunit)]
        [(apair (car rls) (racketlist->mupllist (cdr rls)))]
        )
  )

(define (mupllist->racketlist mls)
  (cond [(aunit? mls) '()]
        [(cons (apair-e1 mls) (mupllist->racketlist (apair-e2 mls)))]
        )
  )
;; CHANGE (put your solutions here)
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
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
       	[(int? e) e]
	[(aunit? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
        (let ([v1 (eval-under-env (ifgreater-e1 e) env)] [v2 (eval-under-env (ifgreater-e2 e) env)]);"evaluates its first two subexpressions to values v1 and v2 respectively"
        (if (and (int? v1) (int? v2));both ints evaluate to true
           ;then
            (if (> (int-num v1) (int-num v2))
               ;if v1>v2 then eval v3
               (eval-under-env (ifgreater-e3 e) env)
               ;else evaluate the fourth expression
               (eval-under-env (ifgreater-e4 e) env)
               ;close
             )
           ;else: no condition specified, so void
           (void));endif
          )]
        
         [(mlet? e) (let ([v (eval-under-env (mlet-e e) env)])
                    ;An mlet expression evaluates its first expression to a value v.
                    ;Then the body of the let
                    ;Wherein: it evaluates the second expression
                    ;of (mlet-var) (mlet-e) (mlet-body) to a value
                    ;i.e. (mlet-e)
                    ;but in a new environment extended
                    ;to map the name in the mlet expression to v.
                    ;i.e. env must be changed, so
                    ;extended
                    ;;
                    ; map the name
                    ;;
                    ;in the mlet expression
                    ;;
                    ;to v
                    ;;(list (cons (mlet-var e) v)) env)))
                    (eval-under-env (mlet-body e) (append (list (cons (mlet-var e) v)) env))
                    ;(eval-under-env (mlet-body e) (list (cons (mlet-var e) v) env))
              ;close let
              )
              ;close mlet
              ]
       	[(call? e)
	 (let ([values1 (eval-under-env (call-funexp e) env)]
	       [values2 (eval-under-env (call-actual e) env)])
	   (if (not (closure? values1))
               (display "error");If the first is not a closure, it is an error
               ;Else, it evaluates the closure’s function’s body:
	       (eval-under-env (fun-body (closure-fun values1))
             ;(fun-body (closure-fun values1))
               ;in the closure’s
               ;environment extended to map the function’s name
             ;(fun-nameopt (closure-fun values))
               ;to the closure
	       (append (closure-env values1)
	       (list (cons (fun-formal (closure-fun values1)) values2)
               ;(unless the name field is #f)
               (if (not (fun-nameopt (closure-fun values1)))
               '()
               ;else map
               (cons (fun-nameopt (closure-fun values1)) values1)
                                                 ))))
               ;and the function’s argument-name (i.e.,
               ;the parameter name) to the result of the second subexpression
               ;close if
               )
               ;close let
               )
               ;close call
               ]
	     
	 [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
	       [v2 (eval-under-env (apair-e2 e) env)])
           ;body
          (apair v1 v2))]
	[(fst? e)
	 (let ([value (eval-under-env (fst-e e) env)])
	   (if (apair? value) (apair-e1 value) (void)))]
	[(snd? e) (let ([value (eval-under-env (snd-e e) env)])
	   (if (apair? value) (apair-e2 value) (void)))]
	[(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
	   (if (aunit? v) (int 1) (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))


;;
;> (struct mypair (e1 e2))
;> (struct fst (e))
;> (define mymypair (mypair "em" "am"))
;> (define myfst mymypair)
;> (fst? myfst)
;#f
;> (mypair? myfst)
;#t
;> (define mylefst (mymypair));
;alication: not a procedure;
; expected a procedure that can be applied to arguments
;  given: #<mypair>;
;  arguments...: [none]
;> (define myfirstfst (fst mymypair))
;> (fst? myfirstfst)
;#t
;> (fst-e myfirstfst)
;#<mypair>
;> (mypair? (fst-e myfirstfst))
;#t
;> 

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) 
  (cond [(aunit? e1) e2]
        [(not (aunit? e1)) e3]))

;ALL DOWN TO HERE SEEM FINE RIGHT NOW

;Write a Racket function mlet*
;that takes a Racket list of Racket pairs
;lstlst (s.e) and a final mupl expression e2.
;In each pair, assume si is a Racket string
(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        ;i.e. (car lstlst) yields a pair
        ;each pair, (car pair) is si
        ;so mlet si is mlet (car (car )

;and ei is a mupl expression.
        ;e is then car first then cdr of pair
        ;i.e. (cdr (car lst))

;mlet* returns
;a mupl expression whose value is en+1 evaluated
;in an environment where each si
        ;i.e. each (car (car ))
;is a variable
        ;so  we bind each (car (car))
;bound to the result of evaluating the
;corresponding ei for 1 ≤ i ≤ n.
               ;i.e. the (cdr (car))s
               ;which makes mlet val-e-body be
               ;mlet (car (car)) (cdr (car)) body:

        ;[(mlet (car (car lstlst)) (cdr (car lstlst))
        ;body
        
               
;The bindings
;are done sequentially, so that each ei is
;evaluated in an environment where s1 through
;si-1 have been previously bound to the values
;e1 through ei-1.
               
         ;i.e. the body becomes 'call mlet* again
         ;to run mlet again with (cdr lstlst))' 
      ;body=(mlet* (cdr lstlst) e2))]

      [(mlet (car (car lstlst)) (cdr (car lstlst))  (mlet* (cdr lstlst) e2))]
      )
  )

;Write a Racket function ifeq that takes four
;mupl expressions e1, e2, e3, and e4 and returns
(define (ifeq e1 e2 e3 e4)
  
;a mupl expression that acts like ifgreater except
;e3 is evaluated if and only if e1 and e2 are equal
    ;so first check e1 e2 eq integers
;;;    (if
;;;    (and (int? (eval-exp e1)) (int? (eval-exp e2)))
;;;    (int 99)
;;;    (int 33)))
;;;> (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4)))
;;;(int 99)
;;;        (if
;;;        (and (int? (eval-exp e1)) (int? (eval-exp e2)))
;;;        then
;;;        else

;Assume none of the arguments to ifeq use
;the mupl variables _x or _y.
     ;?????

        (let ([v1 (eval-exp e1)];eval e1 only once
              [v2 (eval-exp e2)]);;eval e2 only once
        (if
        (and (int? v1) (int? v2))
          (if (> (int-num v1) (int-num v2))
              (eval-exp e3)
              (eval-exp e4)
        ;if they are equal integers but ifgreater?... Ah!
        ;if eq then is e3, else is e4
        )
        ;else
        (void))
        ;let
        )
  )

;; Problem 4

;Bind to the Racket variable mupl-map 
(define mupl-map
;a function that acts like map
 (fun "muplfunmap" "arg1"
;it should take a mupl function
   (fun "mupllistmap" "arg2"
   ;(s1.e1)      (s1.e2)
  (mlet*
   ;that takes a mupl lstlst
  (list (cons "ss_in_pair" (isaunit (var "arg2"))) (cons "es_in_pair" (int 1)))
  ;takes e2
  ;Recall
  ;a mupl list is aunit or a pair where
  ;the second component is a mupl list.
  (ifgreater (var "ss_in_pair") (var "es_in_pair")
    ;then
    (apair (call (var "arg1") (fst (var "arg2")))
          (call (var "mupllistmap") (snd (var "arg2"))))
     ;else
     (ifgreater (var "es_in_pair") (var "ss_in_pair")
     ;then
     (apair (call (var "arg1") (fst (var "arg2")))
	    (call (var "mupllistmap") (snd (var "arg2"))))
      ;else
      (aunit));closeinnerif
     );closeoutif
     ;close mlet
     )
     ;rest
     )))
 

    
;it should take a mupl
; (ifeq (isaunit (var "lst"))
; (int 1)   	 (aunit)

;(define mupl-mapAddN 
;  (mlet "map" mupl-map
;         "CHANGE (notice map is now in MUPL scope)"))

;Bind to Racket variable mupl-mapAddN 
(define mupl-mapAddN
;Use mupl-map (a use of
;mlet is given to you to make this easier).
 (mlet "map" mupl-map
;a mupl function that takes an mupl integer i
 (fun "innerfunint" "i"
;and returns a mupl function that takes a mupl list
 (fun "innerinnerfunint" "innerlist"
;and returns a new mupl list of mupl integers that adds i to every element of the list.     
  (call (call (var "map")
   (fun "adds-i" "element" (add (var "element") (var "i")))) (var "innerlist"))
 )
 )
 ;close mlet
 )
)

;; Challenge Problem
;no :) 
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
