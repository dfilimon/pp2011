;;
(define sum2
  (lambda (x)
    (lambda (y)
      (+ x y) )))

;; (sum2 1) = o funcție, închidere funcțională

;; partial-sum
(define partial-sum (sum2 1))

;; factorialul
(define fact
  (lambda (n)
    (if (= n 0) 1
	(* n (fact (- n 1))) )))

;; (v2) + tail recursion
(define fact2
  (lambda (n)
    (let fact-aux ((n n) (acc 1))
      (if (= n 0) acc
	  (fact-aux (- n 1) (* n acc))) )
    )
  )

(define prod-list
  (lambda (l)
    (if (null? l) 1
	(* (car l) (prod-list (cdr l)))
	)
    )
  )

(define sum-list
  (lambda (l)
    (if (null? l) 0
	(+ (car l) (sum-list (cdr l)))
	)
    )
  )

;; testing a let... sum + prod of
(define sp
  (lambda (l)
    (let ( (s (sum-list l))
	   (p (prod-list l)) )
      (+ s p))
    )
  )

;;
(define a 1)
(define f (lambda() a))
(define g a)
(define a 2)
(f) ; va întoarce 2!
g
;; subtilitate: funcția f se evaluează la ea însăși
;; corpul ei nu va fi _evaluat_ pentru a înlocui a cu 1
;; dar, la aplicare, când se fac toate înlocuirile, a va fi setat pe 2

;; try this... cu curry
;; fold (lambda (acc head-l) (+ acc head-l)) 0 '(1 2 3 4))