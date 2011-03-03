;; definim nor = not or
;; not 1 _ = 0
;; not 0 y = not y
;;

(define nor
  (lambda (x y)
    (not (or x y))
    )
  )

;; (nor 'x nu ar merge :(
;; (eval x ... x va fi rupt de valoarea lui inițială

;; (nor (lambda ()
;; vom avea o funcție care se evaluează la x
;; x și y vor fi îmbrăcate în funcții anonime
;; încerc să evit evaluarea parametrilor

(define norr
  (lambda (x y)
    (if (x) #f
	(not (y)))
    )
  )

;; promisiuni în scheme
(define p
  (delay (+ 1 2)))

(define nor2
  (lambda (x y)
    (if (force x) #f
	(not (force y)))
    )
  )

;; evaluare strictă
(define Fix
  (lambda (f)
    (f (Fix f))))

(define g
  (lambda (x) 1))

;; 3
;; ;Aborting!: maximum recursion depth exceeded


;; stream: listă infinită
(define ones
  (cons 1 (delay ones)))
					; ;Value 3: (1 . #[promise 2])

(define take
  (lambda (n s)
    (if (= n 0) '()
	(cons (car s) (take (- n 1) (force (cdr s)))))
    )
  )

;;
(define twos
  (cons 2 (lambda () twos)))

(define take2
  (lambda (n s)
    (if (= n 0) '()
	(cons (car s) (take2 (- n 1) ((cdr s))))
	)
    )
  )

;; (define integers
;;   (let integers-stream ((i 0))
;;     (cons i (delay (integers-stream (+ i 1))))
;;     (integers-stream 0)))


(define make-naturals
  (lambda (n)
    (cons n (delay (make-naturals (+ n 1))))
    )
  )

(define naturals
  (make-naturals 0))

(define make-stream
  (lambda (e0 f)
    ( cons e0 (delay (make-stream (f e0) f)) )
    )
  )

(define naturals2
  ( make-stream 0 (lambda (x) (+ x 1)) )
  )

(define add-streams
  (lambda (s1 s2)
    (cons (+ (car s1) (car s2))
	  (delay (add-streams (force (cdr s1))
			      (force (cdr s2)) )))
    )
  )

(define fibo-stream
  (cons 0
	(delay (cons 1
		     (delay (add-streams
			     fibo-stream
			     (force (cdr fibo-stream)))
			    )
		     )
	       )
	)
  )

;; (define prime?
;;   (lambda (n prime-list)
;;     (if (null? prime-list) #t
;; 	(if (= (remainder n (car prime-list)) 0) #f
;; 	    (prime? n (cdr prime-list))
;; 	)
;;     )
;;   )
;; )

(define filter-stream
  (lambda (f s)
    (let (
	  ( rest (delay (filter-stream f (force (cdr s)))) )
	  )
      (if (f (car s)) (cons (car s) rest)
	  (force rest))
      )
    )
  )

(define sieve
  (lambda (s)
    (cons (car s)
	  (delay
	    (sieve
	     (filter-stream (lambda (x) (not (= (remainder x (car s)) 0))) s)
	     )
	  ) )
    )
  )

(define naturals-from-2
  (force
   (cdr (force (cdr naturals))
	)
   )
  )

(define primes
  (sieve naturals-from-2)
  )

