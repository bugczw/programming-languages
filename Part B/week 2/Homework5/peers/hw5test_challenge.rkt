#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "hw5.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"

   ;; find-free-vars test
   (check-equal? (find-free-vars (int 6)) (set) "find-free-vars test0")
   (check-equal? (find-free-vars (aunit)) (set) "find-free-vars test1")
   (check-equal? (find-free-vars (var "x")) (set "x") "find-free-vars test2")
   (check-equal? (find-free-vars (add (var "x") (var "y"))) (set "x" "y") "find-free-vars test3")
   (check-equal? (find-free-vars (fun "f" "x" (add (var "t") (var "m")))) (set "t" "m") "find-free-vars test4")
   (check-equal? (find-free-vars (ifgreater (var "x") (var "y") (var "z") (var "r"))) (set "x" "y" "z" "r") "find-free-vars test5")
   (check-equal? (find-free-vars (call (var "x") (var "y"))) (set "x" "y") "find-free-vars test6")
   (check-equal? (find-free-vars (fun "f" "x" (add (var "x") (var "m")))) (set "m") "find-free-vars test7")
   (check-equal? (find-free-vars (mlet "a" (var "x") (add (var "a") (int 2)))) (set "x") "find-free-vars test8")
   ;; compute-free-vars test
   (check-equal? (compute-free-vars (int 6)) (int 6) "compute-free-vars test0")
   (check-equal? (compute-free-vars (aunit)) (aunit) "compute-free-vars test1")
   (check-equal? (compute-free-vars (var "x")) (var "x") "compute-free-vars test2")
   (check-equal? (compute-free-vars (add (var "x") (var "y"))) (add (var "x") (var "y")) "compute-free-vars test3")
   (check-equal? (compute-free-vars (ifgreater (var "x") (var "y") (var "z") (var "r"))) 
                 (ifgreater (var "x") (var "y") (var "z") (var "r")) "compute-free-vars test4")
   (check-equal? (compute-free-vars (call (var "x") (var "y"))) (call (var "x") (var "y")) "comput3-free-vars test5")
   (check-equal? (compute-free-vars (fun "f" "x" (add (var "x") (var "m"))))  
                 (fun-challenge "f" "x" (add (var "x") (var "m")) (set "m")) "compute-free-vars test6")

   ;; create-free-var-env test
   (check-equal? (create-free-var-env (list) (list))
                 (list) "create-free-var-env test0")
   (check-equal? (create-free-var-env (list "x" "y") (list (cons "x" (int 1)) (cons "z" (aunit)) (cons "y" (apair (int 2) (int 3)))))
                 (list (cons "x" (int 1)) (cons "y" (apair (int 2) (int 3)))) "create-free-var-env test1")
   ;; value test
   (check-equal? (eval-exp-c (int 2)) (int 2) "int test")
   (check-equal? (eval-exp-c (closure (list) (fun #f "x" (var "x")))) (closure (list) (fun #f "x" (var "x"))) "closure test")
   (check-equal? (eval-exp-c (aunit)) (aunit) "aunit test")
   (check-equal? (eval-exp-c (apair (int 2) (aunit))) (apair (int 2) (aunit)) "apair test")

   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp-c (ifgreater (int 3) (int 3) (int 3) (int 2))) (int 2) "ifgreater test1")
   (check-equal? (eval-exp-c (ifgreater (int 3) (int 2) (int 3) (int 2))) (int 3) "ifgreater test2")

   ;; fun test
   (check-equal? (eval-exp-c (fun "f" "x" (var "x"))) (closure (list) (fun "f" "x" (var "x"))) "fun test")

   ;; mlet test
   (check-equal? (eval-exp-c (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   
   ;; call test
   (check-equal? (eval-exp-c (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   
   ;;pair test
   (check-equal? (eval-exp-c (apair (add (int 3) (int 1)) (int 2))) (apair (int 4) (int 2)) "apair test")
   (check-equal? (eval-exp-c (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   (check-equal? (eval-exp-c (fst (apair (int 1) (int 2)))) (int 1) "fst test")
   
   ;; isaunit test
   (check-equal? (eval-exp-c (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test1")
   (check-equal? (eval-exp-c (isaunit (aunit))) (int 1) "isaunit test2")

   ;; ifaunit test
   (check-equal? (eval-exp-c (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test1")
   (check-equal? (eval-exp-c (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test2")

   ;; mlet* test
   (check-equal? (eval-exp-c (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test1")
   (check-equal? (eval-exp-c (mlet* (list (cons "x" (int 1)) (cons "y" (add (int 2) (var "x")))) (var "y"))) (int 3) "mlet* test2")
   
   ;; ifeq test
   (check-equal? (eval-exp-c (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test1")
   (check-equal? (eval-exp-c (ifeq (int 1) (int 1) (int 3) (int 4))) (int 3) "ifeq test2")

   ;; fibonacci test
   ;(check-equal? (eval-exp-c (call fib (int 5))) (int 15) "fibonacci test")

   ;; mupl-map test
   (check-equal? (eval-exp-c (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")
   (check-equal? (eval-exp-c (call (call mupl-map (fun #f "x" (apair (var "x") (var "x")))) (apair (int 1) (apair (int 2) (aunit))))) 
                 (apair (apair (int 1) (int 1)) (apair (apair (int 2) (int 2)) (aunit))) "mupl-map test")

   ;; mupl-mapAddN test
   (check-equal? (eval-exp-c (call (call mupl-mapAddN (int 3)) (apair (int 1) (apair (int 2) (aunit))))) 
                 (apair (int 4) (apair (int 5) (aunit))) "mupl-mapAddN test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp-c (call (call mupl-mapAddN (int 7))
                     (racketlist->mupllist
                       (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
