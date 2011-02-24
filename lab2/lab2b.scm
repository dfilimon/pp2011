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