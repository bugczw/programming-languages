#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;;Problem 1

(define (sequence low high stride)
   (if (<= low high) 
       (cons low (sequence (+ low stride) high stride))
	   null))
	   
	   
	   
;;Problem 2

(define (string-append-map xs suffix)
    (map (lambda(s) (string-append s suffix)) xs))
	
	
	
;;Problem 3

(define (list-nth-mod xs n)
   (cond[(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
		[#t (car (list-tail xs (remainder n (length xs))))]))

		

;;Problem 4

(define (stream-for-n-steps s n)
    (if (<= n 0) null
	    (cons (car (s)) (stream-for-n-steps  (cdr (s)) (- n 1)))))
		
		
		
;;Problem 5

(define funny-number-stream 
    (letrec ([f (lambda(n) (cons (if (= (remainder n 5) 0) (- 0 n) n) (lambda() (f (+ n 1)))))])
	         (lambda() (f 1))))
	
	
	
;;Problem 6

(define dan-then-dog
    (letrec ([dan? (lambda() (cons "dan.jpg" dog?))]
	         [dog? (lambda() (cons "dog.jpg" dan?))])
			 dan?))
			 
			 

;;Problem 7

(define (stream-add-zero s)
   (lambda() (cons (cons 0 (car(s))) (stream-add-zero (cdr(s))))))
   
   
 
 
 
;;Problem 8

(define (cycle-lists xs ys)
    (letrec([f (lambda(n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda() (f (+ n 1)))))])
	        (lambda() (f 0))))

			
			
;;Problem 9
			
(define (vector-assoc v vec)
    (letrec ([veclen (vector-length vec)]
	         [helper (lambda(n)
			          (if (= n veclen) #f
					      (let* ([element (vector-ref vec n)])
						         (if (and (pair? element) (equal? (car element) v))
								      element (helper (+ n 1))))))])
	(helper 0)))
	
	
	
	
	
	
;;Problem 10

(define (cached-assoc xs n)
    (letrec ([cache (make-vector n #f)]
	         [ptr 0]
			 [f (lambda(i v)
			      (if (= i n) (let ([newval (assoc v xs)])
				              (begin (vector-set! cache ptr newval)
							         (set! ptr (remainder (+ ptr 1) n))
									 newval))
							  (let ([element (vector-ref cache i)])
							       (if (and (pair? element) (equal? (car element) v))
								     element (f (+ i 1) v)))))])
	(lambda(v) (f 0 v))))


	
	
;;Challenge Problem

(define-syntax while-less
  (syntax-rules (do)
    [(while-less exp1 do exp2)
     (letrec ([e1 exp1]
              [e2 (lambda () exp2)]
              [loop (lambda(x-thunk)
                      (let ([x (x-thunk)])
                        (if (or (not (number? x)) (>= x e1))
                            #t (loop x-thunk))))])
     (loop e2))]))


