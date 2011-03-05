;; SICP, section 1.2
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2

;; 1.2.4
;; exponentiation

;; linear recursive process, takes Theta(n) steps & space
(define (expt b n)
  (if (= n 0) 1
      (* b (expt b (- n 1)))))

;; equivalent linear iteration
(define (expt2 b n)
  (letrec
      (
	(expt2-acc
	 (lambda (acc i)
	   (if (= i 0) acc
	       (expt2-acc (* acc b) (- i 1))))
	 )
	)
    (expt2-acc 1 n)
    )
  )

;; named-let version, I like this one the most
(define expt3
  (lambda (b n)
    (let expt3-acc ((acc 1) (i n))
      (if (= i 0) acc
	  (expt3-acc (* acc b) (- i 1))))))

;; fast exponentiation
(define fast-expt
  (lambda (b n)
    (cond ((= n 0) 1)
	  ((even? n) (square (fast-expt b (quotient n 2))))
	  (else (* b (fast-expt b (- n 1))))
	  )
    )
  )

;; very fast (log n) versus (n)

;; 1 ]=> (let ( (start-time (runtime)) ) (begin (fast-expt 4 1000000) (- (runtime) start-time)))
;; Value: 2.37


;; testing for primality

;; checking a number's divisiors
(define divides?
  (lambda (a b)
    (= (remainder a b) 0)))

(define prime?
  (lambda (n)
    (cond ((or (= n 1) (= n 0)) #f)
	  ((= n 2) #t)
	  ((even? n) #f)
	  (else (let prime-test ( (d 3) )
		  (cond ((> (square d) n) #t)
			((divides? n d) #f)
			(else (prime-test (+ d 2)))))))))


;;;
;; The Fermat test
;; (log n)
;;
;; Fermat's Little Theorem: If n is a prime number and a is any positive integer
;; less than n, then a raised to the nth power is congruent to a modulo n.
;;;

;; not only is it probabilistic and needs multiple runs to ensure a high
;; likelihood of success, but also it flat out fails for Carmichael numbers

;;5 error> (fermat-prime? 1105)
;Value: #t
;;5 error> (prime? 1105)
;Value: #f

(define expmod
  (lambda (base exp mod)
    (remainder
     (cond ((= exp 0) 1)
	   ((even? exp) (square (expmod base (/ exp 2) mod)))
	   (else (* base (expmod base (- exp 1) mod))))
     mod)))


;;;
;; The Miller-Rabin test
;; Starts from an alternate form of Fermat's Little Theorem: if n is a prime
;; number and a is any positive integer less than n, then a raised to the
;; (n-1)st power is congruent to 1 modulo n.
;;
;; Key Difference: whenever performing the squaring step in expmod (!!), check
;; to see if we've discovered a nontrivial square root of 1 modulo n, a number
;; not equal to 1 or n - 1 whose square is equal to 1 modulo n. It is possible
;; to prove that if such a number exists, then n is not prime.
;; Also, if n is an odd number that is not prime, fol at least half of numbers
;; a < n, will running the test yield a nontrivial square root of 1.
;;;

(define sqrt-of-1?
  (lambda (s n)
    (if (or (= s 1) (= s (- n 1))) #f
	(= (remainder (* s s) n) 1))))

(define expmod2
  (lambda (base exp mod)
    (remainder
     (cond ((= exp 0) 1)
	   ((even? exp)
	    (let ((s (expmod2 base (/ exp 2) mod)))
	     (if (sqrt-of-1? s mod) 0
		 (* s s))))
	   (else (* base (expmod2 base (- exp 1) mod))))
     mod)))

(define rabinmiller-prime?
  (lambda (n)
    (if (or (= n 0) (= n 1)) #f
	(let rabinmiller-tests ((i 5))
	  (if (= i 0) #t
	      (let ( (a (+ 1 (random (- n 1)))) )
		(if (= (expmod2 a (- n 1) n) 1) (rabinmiller-tests (- i 1))
		    #f)))))))

;; inclusive range
(define range
  (lambda (a b)
    (if (> a b) '()
	(cons a (range (+ a 1) b)))))

(define rabinmiller-true-prime?
  (lambda (n)
    (cond ((or (= n 0) (= n 1)) #f)
	  ((= n 2) #t)
	  ((even? n) #f)
	  (else
	   (let rabinmiller-tests ( (test-range (range 2 (min (- n 1) (* 2 (square (floor (log n))))))) )
	     (if (null? test-range) #t
		 (let ((a (car test-range)))
		   (if (= (expmod2 a (- n 1) n) 1) (rabinmiller-tests (cdr test-range))
		       #f))))))))


;; toy benchmark
(define benchmark
  (lambda (f)
    (let ( (start-time (runtime)) )
      (begin
	(force f)
	(- (runtime) start-time)
      ))))

;; fun with streams

(define ones
  (cons-stream 1 ones))

(define twos
  (cons-stream 2 twos))

(define naturals
  (let make-naturals ((i 0))
    (cons-stream i (make-naturals (+ i 1)))))

;; (define filter-stream
;;   (lambda (f s)
;;     (let ( (rest (filter-stream f (stream-cdr s)))
;; 	   (head (stream-car s)) )
;;       (if (f head) (cons-stream head (delay rest))
;; 	  (delay rest)))))

(define filter-stream
  (lambda (f s)
    (let ( (rest (delay (filter-stream f (force (cdr s)))))
	   (head (car s)) )
      (if (f head) (cons head rest)
	   (force rest)))))

(define take
  (lambda (n s)
    (if (= n 0) '()
	(let ( (rest (take (- n 1) (stream-cdr s)))
	       (head (stream-car s)) )
	  (cons head rest)))))

;; 1 ]=> (benchmark (delay (take 1000 (filter-stream prime? naturals))))

;; ;Value: .17000000000001592

;; 1 ]=> (benchmark (delay (take 10000 (filter-stream prime? naturals))))

;; ;Value: 4.850000000000023

;; 2 error> (benchmark (delay (take 10000 (filter-stream rabinmiller-prime? naturals))))

;; ;Value: 9.710000000000036

(define take-while
  (lambda (f s)
    (if (not (f (car s))) '()
	(cons (car s) (take-while f (force (cdr s)))))))

;; (define sieve2
;;   (lambda (s)
;;     (let ( (rest (delay (sieve2 (force (cdr s)))))
;; 	   (head (car s)) )
;;       (if (sieve-prime? head) (cons (car s) rest)
;; 	  (force (rest))))))

;; (define sieve-prime?
;;   (lambda (n)
;;     (letrec ( (divides-any?
;; 	       (lambda (n l)
;; 		 (if (null? l) #f
;; 		     ) )
;;     (take-while (lambda (x) (< (* x x) n) primes))