#lang racket

(require "extra5.rkt")
(require rackunit)

(define tests
  (test-suite
   "Tests for practice problems in extra5.rkt"

   
    ; tree-height tests

   (check-equal?  
       (tree-height (btree-leaf))
    0
    "tree-height test1")

   (check-equal?  
       (tree-height (btree-node #t (btree-leaf) (btree-leaf)))
    1
    "tree-height test2")

   (check-equal?
       (tree-height 
	       (btree-node #t
               (btree-leaf)
               (btree-node #t
                   (btree-leaf)
                   (btree-leaf))))
    2
    "tree-height test3")

   (check-equal?
       (tree-height 
	       (btree-node #t
               (btree-node #t
                   (btree-node #t
                       (btree-leaf)
                       (btree-leaf))
                   (btree-node #t
				       (btree-leaf)
				       (btree-node #t
					        (btree-leaf)
							(btree-leaf))))
               (btree-node #t
                   (btree-node #t
				       (btree-leaf)
				       (btree-leaf))
                   (btree-node #t
			           (btree-leaf)
				       (btree-leaf)))))
    4
    "tree-height test4")


	
	
   ; sum-tree tests
   (check-equal? (sum-tree (btree-leaf))
    0
    "sum-tree test1")

   (check-equal?
    (sum-tree (btree-node 1
                          (btree-leaf)
                          (btree-leaf)))
    1
    "sum-tree test1")

   (check-equal?
    (sum-tree (btree-node 1
                          (btree-leaf)
                          (btree-node 2
                                      (btree-leaf)
                                      (btree-leaf))))
    3
    "sum-tree test2")

   (check-equal?
    (sum-tree (btree-node 1
                          (btree-node 3
                                      (btree-leaf)
                                      (btree-leaf))
                          (btree-node 2
                                      (btree-leaf)
                                      (btree-leaf))))
    6
    "sum-tree test3")

   (check-equal?
        (sum-tree 
		    (btree-node 1                         		  
			    (btree-node 2
                    (btree-node 4
                            (btree-leaf)
                            (btree-leaf))
                    (btree-node 5
                        (btree-leaf)
                        (btree-leaf)))
                (btree-node 3
                    (btree-node 6
                        (btree-leaf)
                        (btree-leaf))
                    (btree-node 7
                        (btree-leaf)
                        (btree-leaf)))))
    28
    "sum-tree test4")

	
	
   ; prune-at-v tests
   (let ([tree (btree-node 1
                          (btree-node 2
                                      (btree-node 3
                                                  (btree-leaf)
                                                  (btree-leaf))
                                      (btree-node 4
                                                  (btree-leaf)
                                                  (btree-leaf)))
                          (btree-node 3
                                      (btree-node 5
                                                  (btree-leaf)
                                                  (btree-leaf))
                                      (btree-node 5
                                                  (btree-leaf)
                                                  (btree-leaf))))]
         [tree-pruned-at-1 (btree-leaf)]
         [tree-pruned-at-2 (btree-node 1
                                       (btree-leaf)
                                       (btree-node 3
                                                   (btree-node 5
                                                               (btree-leaf)
                                                               (btree-leaf))
                                                   (btree-node 5
                                                               (btree-leaf)
                                                               (btree-leaf))))]
         [tree-pruned-at-3 (btree-node 1
                                       (btree-node 2
                                                   (btree-leaf)
                                                   (btree-node 4
                                                               (btree-leaf)
                                                               (btree-leaf)))
                                       (btree-leaf))]
         [tree-pruned-at-4 (btree-node 1
                                       (btree-node 2
                                                   (btree-node 3
                                                              (btree-leaf)
															  (btree-leaf))                             
                                      (btree-leaf))
                          (btree-node 3
                                      (btree-node 5
                                                  (btree-leaf)
                                                  (btree-leaf))
                                      (btree-node 5
                                                  (btree-leaf)
                                                  (btree-leaf))))]
         [tree-pruned-at-5 (btree-node 1
                                       (btree-node 2
                                                   (btree-node 3
                                                               (btree-leaf)
                                                               (btree-leaf))
                                                   (btree-node 4
                                                               (btree-leaf)
                                                               (btree-leaf)))
                                       (btree-node 3
                                                   (btree-leaf)
                                                   (btree-leaf)))])   
     (check-equal? (prune-at-v tree 1) tree-pruned-at-1 "prune-at-v test1")
     (check-equal? (prune-at-v tree 2) tree-pruned-at-2 "prune-at-v test2")
     (check-equal? (prune-at-v tree 3) tree-pruned-at-3 "prune-at-v test3")
     (check-equal? (prune-at-v tree 4) tree-pruned-at-4 "prune-at-v test4")
     (check-equal? (prune-at-v tree 5) tree-pruned-at-5 "prune-at-v test5"))


   ; well-formed-tree tests
   (check-equal? (well-formed-tree? #t) #f "#t is not a well-formed tree")
   (check-equal? (well-formed-tree? (btree-leaf)) #t "well-formed btree-leaf")
   (check-equal? (well-formed-tree? (btree-node #t #t #t)) #f "bad-formed btree-node")
   (check-equal? (well-formed-tree? (btree-node #t (btree-leaf) (btree-leaf))) #t "well-formed btree-node")
   (check-equal? (well-formed-tree?
                  (btree-node 1
                              (btree-node 2
                                          (btree-node 3
                                                      (btree-leaf)
                                                      (btree-leaf))
                                          (btree-node 4
                                                      (btree-leaf)
                                                      (btree-leaf)))
                              (btree-node 3
                                          (btree-node 5
                                                      (btree-leaf)
                                                      (btree-leaf))
                                          (btree-node 5
                                                      (btree-leaf)
                                                      #t))))
                 #f
                 "deeply nested bad-formed btree-node")


   ; fold-tree tests
   (check-equal? (fold-tree (lambda (x y) (+ x y 1))
                            99
                            (btree-leaf))
                 99
                 "fold-tree returns acc for btree-leaf")
   
   (check-equal? (fold-tree (lambda (x y) (+ x y 1))
                            7
                            (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf)))
                 18
                 "fold-tree folds btree-node test")

   (check-equal? (fold-tree (lambda (x y) (* x y))
                            7
                            (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf)))
                 140
                 "fold-tree folds btree-node test")



   ; curried-fold-tree tests
   (check-equal? ((((curried-fold-tree) (lambda (x y) (+ x y 1)))
                            99)
                            (btree-leaf))
                 99
                 "fold-tree returns acc for btree-leaf")
   
   ; crazy-sum tests
   (check-equal? (crazy-sum (list 10 * 6 / 5 - 3))
                 9 "crazy-sum test")

   (check-equal? (crazy-sum (list 1 2 3 4 5 6 7))
                 28 "crazy-sum sums if no function in list")

   (check-equal? (crazy-sum null)
                 0 "crazy-sum returns 0 for empty list")


   ; either-fold tests

   (check-equal? (either-fold (lambda (x y) (+ x y)) 0 null)
                 0 "either-fold folds empty list")

   (check-equal? (either-fold (lambda (x y) (+ x y)) 0 (list 1 2 3))
                 6 "either-fold folds non empty list")

   (check-equal? (either-fold (lambda (x y) (+ x y)) 0 (btree-leaf))
                 0 "either-fold folds btree-leaf")

   (check-equal? (either-fold (lambda (x y) (+ x y)) 1 (btree-node 5
                                                                   (btree-leaf)
                                                                   (btree-node 7
                                                                               (btree-leaf)
                                                                               (btree-leaf))))
                 13 "either-fold folds btree-node")

   (check-exn exn:fail? (lambda()
                          (either-fold (lambda (x y) (+ x y)) 0 (vector null)))
              "either-fold raises error if not a list or btree")
			  
   (check-exn exn:fail? (lambda()
                          (either-fold (lambda (x y) (+ x y)) 0 (btree-node 5 6 7)))
              "either-fold raises error if not a list or btree")


   ; flatten tests

   (check-equal? (flatten null)
                 null
                 "flatten flattens empty list")

   (check-equal? (flatten (list 1 2 3 4 5 6 7 8 9 10))
                 (list 1 2 3 4 5 6 7 8 9 10)
                 "flatten flattens flat list")

   (check-equal? (flatten (list 1 2 (list (list 3 4) 5 (list (list 6) 7 8)) 9 (list 10)))
                 (list 1 2 3 4 5 6 7 8 9 10)
                 "flatten flattens nested list")

				 
    ; simplify tests
	
	(let* ([e (apair (int 1) (apair (int 2) (apair (int 3) (apair (int 4) (apair (int 5) (int 6))))))]
	       [e1 (simplify (fst  e))]
		   [e2 (simplify (fst (snd e)))]
		   [e3 (simplify (fst (snd (snd (snd (snd e))))))]
		   [f (call 
		      (fun #f "x" 
			      (mlet "p" 
				      (apair (aunit) (var "x")) 
					  (apair (snd (var "p")) (fst (var "p"))))) 
		      (int 1))]
		   [f1 (simplify (fst f))])
          (check-equal? (eval-exp e1) (int 1) "simplify test1")
		  (check-equal? (eval-exp e2) (int 2) "simplify test2")
		  (check-equal? (eval-exp e3) (int 5) "simplify test3")
		  (check-equal? (eval-exp f1) (int 1) "simplify test4"))
		  
		  
	; mupl-all tests
	
	(let*
	    ([f (fun #f "_x" (ifgreater (var "_x") (int 2) (int 1) (int 0)))]
		 [ml1 (aunit)]
		 [ml2 (apair (int 1) (apair (int 2) (apair (int 3) (apair (int 4) (apair (int 5) (aunit))))))]
		 [ml3 (apair (int 4) (apair (int 5) (apair (int 6) (apair (int 7) (apair (int 8) (aunit))))))])
	    (check-equal? (eval-exp (call (call mupl-all f) ml1)) (int 1) "mupl-all test1")
		(check-equal? (eval-exp (call (call mupl-all f) ml2)) (int 0) "mupl-all test2")
		(check-equal? (eval-exp (call (call mupl-all f) ml3)) (int 1) "mupl-all test3"))

	

    ; mupl-append tests
	
    (let*
	   ([ml1 (aunit)]
	    [ml2 (apair (int 1) (apair (int 2) (aunit)))]
		[ml3 (apair (int 3) (apair (int 4) (apair (int 5) (aunit))))]
		[ml4 (apair (int 6) (aunit))]
		[ml5 (apair (int 2) (apair (int 1) (apair (int 3) (apair (int 4) (apair (int 5) (aunit))))))]
		[ml6 (apair (int 2) (apair (int 1) (apair (int 6) (aunit))))])
	   (check-equal? (eval-exp (call (call mupl-append ml1) ml2)) ml2 "mupl-append test1")
	   (check-equal? (eval-exp (call (call mupl-append ml2) ml3)) ml5 "mupl-append test2")
	   (check-equal? (eval-exp (call (call mupl-append ml2) ml4)) ml6 "mupl-append test3"))
	   
	   
	; mupl-zip tests
	
    (let*
	   ([ml1 (aunit)]
	    [ml2 (apair (int 1) (apair (int 2) (aunit)))]
		[ml3 (apair (int 3) (apair (int 4) (apair (int 5) (aunit))))]
		[ml4 (apair (apair (int 1) (int 3)) (apair (apair (int 2) (int 4)) (aunit)))])
	   (check-equal? (eval-exp (call (call mupl-zip ml1) ml2)) ml1 "mupl-zip test1")
	   (check-equal? (eval-exp (call (call mupl-zip ml2) ml3)) ml4 "mupl-zip test2"))
	   
	   
    ; mupl-append2 tests
	
    (let*
	   ([ml1 (aunit)]
	    [ml2 (apair (int 1) (apair (int 2) (aunit)))]
		[ml3 (apair (int 3) (apair (int 4) (apair (int 5) (aunit))))]
		[ml4 (apair (int 6) (aunit))]
		[ml5 (apair (int 2) (apair (int 1) (apair (int 3) (apair (int 4) (apair (int 5) (aunit))))))]
		[ml6 (apair (int 2) (apair (int 1) (apair (int 6) (aunit))))])
	   (check-equal? (eval-exp (call mupl-append2 (apair ml1 ml2))) ml2 "mupl-append2 test1")
	   (check-equal? (eval-exp (call mupl-append2 (apair ml2 ml3))) ml5 "mupl-append2 test2")
	   (check-equal? (eval-exp (call mupl-append2 (apair ml2 ml4))) ml6 "mupl-append2 test3"))
	   
	   
	   
	; mupl-zip2 tests
	
    (let*
	   ([ml1 (aunit)]
	    [ml2 (apair (int 1) (apair (int 2) (aunit)))]
		[ml3 (apair (int 3) (apair (int 4) (apair (int 5) (aunit))))]
		[ml4 (apair (apair (int 1) (int 3)) (apair (apair (int 2) (int 4)) (aunit)))])
	   (check-equal? (eval-exp (call mupl-zip2 (apair ml1 ml2))) ml1 "mupl-zip2 test1")
	   (check-equal? (eval-exp (call mupl-zip2 (apair ml2 ml3))) ml4 "mupl-zip2 test2"))
	
	
	; mupl-curry tests
    (let*
	   ([ml1 (aunit)]
	    [ml2 (apair (int 1) (apair (int 2) (aunit)))]
		[ml3 (apair (int 3) (apair (int 4) (apair (int 5) (aunit))))]
		[ml4 (apair (apair (int 1) (int 3)) (apair (apair (int 2) (int 4)) (aunit)))])
	   (check-equal? (eval-exp (call (call (call mupl-curry mupl-zip2) ml1) ml2)) ml1 "mupl-curry test1")
	   (check-equal? (eval-exp (call (call (call mupl-curry mupl-zip2) ml2) ml3)) ml4 "mupl-curry test2"))
	
	; mupl-uncurry tests
	 (let*
	   ([ml1 (aunit)]
	    [ml2 (apair (int 1) (apair (int 2) (aunit)))]
		[ml3 (apair (int 3) (apair (int 4) (apair (int 5) (aunit))))]
		[ml4 (apair (apair (int 1) (int 3)) (apair (apair (int 2) (int 4)) (aunit)))])
	   (check-equal? (eval-exp (call (call mupl-uncurry mupl-zip) (apair ml1 ml2))) ml1 "mupl-uncurry test1")
	   (check-equal? (eval-exp (call (call mupl-uncurry mupl-zip) (apair ml2 ml3))) ml4 "mupl-uncurry test2"))
	
	
	; if-greater3 tests
	
	(check-equal? (eval-exp (if-greater3 (int 5) (int 4) (int 3) (int 2) (int 1))) (int 2) "if-greater3 test1")
	(check-equal? (eval-exp (if-greater3 (int 4) (int 5) (int 3) (int 2) (int 1))) (int 1) "if-greater3 test2")
	(check-equal? (eval-exp (if-greater3 (int 1) (int 1) (int 1) (int 2) (int 1))) (int 1) "if-greater3 test3")
	
	
	; call-curried tests
	
   (let*
	   ([f (fun "listadd" "xs" 
	           (fun #f "acc" 
			      (ifaunit (var "xs") (var "acc") 
				      (call (call (var "listadd") (snd (var "xs"))) (add (var "acc") (fst (var "xs")))))))]
	    [ml1 (apair (int 1) (apair (int 2) (aunit)))]
		[ml2 (apair (int 3) (apair (int 4) (apair (int 5) (aunit))))])
	   (check-equal? (eval-exp (call-curried f (list ml1 (int 0)))) (int 3) "call-curried test1")
	   (check-equal? (eval-exp (call-curried f (list ml2 (int 0)))) (int 12) "call-curried test2"))
		
		
		
		
    ))
		
(require rackunit/text-ui)
;; runs the test
(run-tests tests)
