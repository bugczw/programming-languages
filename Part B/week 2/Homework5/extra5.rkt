#lang racket

;; MUPL structs from Section 6 homework assignment in Programming Languages class
(require "hw5.rkt")


;; Racket structs:
;; provided definitions to implement binary trees,

(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)


(define (tree-height bt) 
   (if (btree-leaf? bt)
       0
       (let ([left-height (tree-height (btree-node-left bt))]
	         [right-height (tree-height (btree-node-right bt))])
            (+ 1 
			   (if (> left-height right-height)
			       left-height
				   right-height)))))

(define (sum-tree bt)
   (if (btree-leaf? bt)
       0
       (+ (sum-tree (btree-node-left bt))
	      (sum-tree (btree-node-right bt))
		  (btree-node-value bt))))

(define (prune-at-v t v)
   (if (btree-leaf? t)
       t
	   (let ([value (btree-node-value t)])
	        (if (equal? value v)
			    (btree-leaf)
				(btree-node value
				   (prune-at-v (btree-node-left t) v)
				   (prune-at-v (btree-node-right t) v))))))
				   				   

(define (well-formed-tree? bt)
   (cond [(btree-leaf? bt) #t]
         [(and (btree-node? bt)
		       (well-formed-tree? (btree-node-left bt))
			   (well-formed-tree? (btree-node-right bt))) #t]
		 [#t #f]))
     
(define (fold-tree f acc bt)
      (if (btree-leaf? bt)
          acc
	      (fold-tree f
		     (fold-tree f (f acc (btree-node-value bt)) (btree-node-left bt))
			 (btree-node-right bt))))		 

(define (curried-fold-tree)
  (lambda (f)
    (lambda (acc)
      (lambda (t)
        (if (btree-leaf? t)
            acc
            (fold-tree f
               (fold-tree f (f acc (btree-node-value t)) (btree-node-left t))
                   (btree-node-right t)))))))




				   
;; Dynamic typing:

(define (crazy-sum xs)
  (letrec ([f (lambda (acc op xs)
                  (cond [(null? xs) acc]
				        [(number? (car xs)) (f (op acc (car xs)) op (cdr xs))]
						[#t (f acc (car xs) (cdr xs))]))])
  (f 0 + xs))) 
  

(define (either-fold f acc list-or-tree)
  (cond [(list? list-or-tree) (foldl f acc list-or-tree)]
        [(well-formed-tree? list-or-tree) (fold-tree f acc list-or-tree)]
        [#t (error "not a list or tree")]))

(define (flatten xs)
  (cond [(null? xs) (list)]
        [(list? (car xs)) (append (flatten (car xs)) (flatten (cdr xs)))]
		[#t (cons (car xs) (flatten (cdr xs)))]))



		
		
;; Using lambda-calculus ideas to remove features from MUPL programs:

(define (simplify e)
  (cond [(mlet? e)
         (call (fun #f (mlet-var e) (simplify (mlet-body e))) (simplify (mlet-e e)))]
        [(apair? e)
         (fun #f "_f" (call (call (var "_f") (simplify  (apair-e1 e))) (simplify (apair-e2 e))))]
        [(fst? e)
         (call (simplify (fst-e e)) (fun #f "_x" (fun #f "_y" (var "_x"))))]
        [(snd? e)
         (call (simplify (snd-e e)) (fun #f "_x" (fun #f "_y" (var "_y"))))]
        [(add? e)
         (add (simplify (add-e1 e)) (simplify (add-e2 e)))]
        [(ifgreater? e)
         (ifgreater (simplify (ifgreater-e1 e))
                    (simplify (ifgreater-e2 e))
                    (simplify (ifgreater-e3 e))
                    (simplify (ifgreater-e4 e)))]
        [(fun? e)
         (fun (fun-nameopt e) (fun-formal e)
              (simplify (fun-body e)))]
        [(call? e)
         (call (simplify (call-funexp e))
               (simplify (call-actual e)))]
        [(isaunit? e)
         (isaunit (simplify (isaunit-e e)))]
		[(closure? e) (closure (simplify (closure-env e)) (simplify (closure-fun e)))]
          [(closure? e) (closure (simplify (closure-env e)) (simplify (closure-fun e)))]     
        [#t e]))
		
		

;; More MUPL functions:

(define mupl-all 
  (fun "mupl-all-f" "_f"
      (fun #f "ml"
	      (ifaunit (var "ml") (int 1)
		     (ifgreater (call (var "_f") (fst (var "ml"))) (int 0)
			      (ifgreater (call (call (var "mupl-all-f") (var "_f")) (snd (var "ml"))) (int 0)
				       (int 1) (int 0))
				   (int 0))))))

(define mupl-append
  (fun "mupl-append-f" "ML1" 
      (fun #f "ML2" 
          (ifaunit (var "ML1") (var "ML2")
			  (call (call (var "mupl-append-f") (snd (var "ML1"))) (apair(fst (var "ML1")) (var "ML2")))))))

(define mupl-zip
  (fun "mupl-zip-f" "ML1"
    (fun #f "ML2"
	   (ifaunit (var "ML1")
	        (aunit)
			(ifaunit (var "ML2")
			   (aunit)
	     	   (apair (apair (fst (var "ML1")) (fst (var "ML2"))) 
		              (call (call (var "mupl-zip-f") (snd (var "ML1"))) (snd (var "ML2")))))))))

(define mupl-append2
   (fun "mupl-append2-f" "MLs"
      (mlet "ML1" (fst (var "MLs"))
	      (mlet "ML2" (snd (var "MLs"))
              (ifaunit (var "ML1") (var "ML2")
			      (call (var "mupl-append2-f") 
				        (apair (snd (var "ML1")) (apair (fst (var "ML1")) (var "ML2")))))))))

(define mupl-zip2
  (fun "mupl-zip2-f" "MLs"
    (mlet "ML1" (fst (var "MLs"))
	   (mlet "ML2" (snd (var "MLs"))
	   	 (ifaunit (var "ML1")
	        (aunit)
			(ifaunit (var "ML2")
			   (aunit)
	     	   (apair (apair (fst (var "ML1")) (fst (var "ML2"))) 
		              (call (var "mupl-zip2-f") (apair (snd (var "ML1")) (snd (var "ML2")))))))))))

(define mupl-curry
  (fun #f "mupl-curry-f"
     (fun #f "_x"
	     (fun #f "_y"
            (call (var "mupl-curry-f") (apair (var "_x") (var "_y")))))))

(define mupl-uncurry
  (fun #f "mupl-uncurry-f"
     (fun #f "_xy"
         (call (call (var "mupl-uncurry-f") (fst (var "_xy"))) (snd (var "_xy"))))))			



		 
;; More MUPL macros:

(define-syntax if-greater3
  (syntax-rules ()
    [(if-greater3 e1 e2 e3 e4 e5)
	 (mlet "_x" e1
	    (mlet "_y" e2
		  (mlet "_z" e3
		     (ifgreater (var "_x") (var "_y")
	            (ifgreater (var "_y") (var "_z") e4 e5) e5))))]))
				
				

(define-syntax call-curried
  (syntax-rules (list)
    [(call-curried e1 (list ea eb))
	   (call (call e1 ea) eb)]))
