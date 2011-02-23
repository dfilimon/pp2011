; suma elementelor unei liste

(define sum_list
  (lambda (l)
    (if (null? l) 0
        (+ (car l) (sum_list (cdr l)))
        )
    )
  )

; trec de la forma uncurried la forma curried
; uncurried: funcție cu mai mulți parametri
; curried: funcție care primește câte un parametru, și
; și în urma aplicării rezultă o funcție parțială care primește
;; următorul parametru
; ...

;; (define u->c
;;   (lambda (f)
;;     (lambda (x)
;;       (lambda (y)

;; f: a x b -> c => f: a -> (b -> c)
(define curry2
  (lambda (f)
    (lambda (x)
      (lambda (y)
	(f x y)
	)
      )
    )
  )

;; f: a -> (b -> c) => f: a x b -> c
;; trec la forma uncurried
(define uncurry2
  (lambda (f)
    (lambda (x y)
      ((f x) y)
      )
    )
  )

; (mapp (lambda (x) (+ x 1)) l)
(define mapp
  (lambda (f l)
    (if (null? l) '()
        (cons (f (car l)) (mapp f (cdr l)))
        )
    )
  )

; (filterr (lambda (x) (> x 3)) l)
(define filterr
  (lambda (f l)
    (if (null? l) '()
        (let ( (lp (filterr f (cdr l))) )
           (if (f (car l)) (cons (car l) lp)
               lp
               )
           )
        )
    )
  )

(define insert
  (lambda (x l)
    (if (null? l) (cons x '())
        (if (> x (car l)) (cons (car l) (insert x (cdr l)))
            (cons x l)
            )
        )
    )
  )

(define insertion-sort
  (lambda (l)
    (if (null? l) '()
        (insert (car l) (insertion-sort (cdr l)))
        )
    )
  )

(define first-n
  (lambda (l n)
    (let first-naux ((l l) (i 0))
      (if (>= i n) '()
	  (cons (car l) (first-naux (cdr l) (+ i 1)))
	  )
      )
    )
  )

(define last-n
  (lambda (l m n)
    (let ( (ignore (- m n)) )
      (let last-naux ((l l) (i 0))
	(if (< i ignore) (last-naux (cdr l) (+ i 1))
	    l)
	)
      )
    )
  )

(define merge
  (lambda (l1 l2)
    (cond ((null? l1) l2)
	  ((null? l2) l1)
	  ((< (car l1) (car l2))
	   (cons (car l1) (merge (cdr l1) l2))
	   )
	  (else
	   (cons (car l2) (merge l1 (cdr l2)))
	   )
	  )
    )
  )

(define merge-sort
  (lambda (l)
    (let ( (n (length l)) )
      (if (null? (cdr l)) l
	  (let* ( (n2 (quotient n 2))
		  (left (first-n l n2))
		  (right (last-n l n (- n n2))) )
	    (merge (merge-sort left) (merge-sort right))
	    )
	  )
      )
    )
  )

;; 1 ]=> l

;; ;Value 3: (4 5 12 5 1 2 3)

;; 1 ]=> (merge-sort l)

;; ;Value 9: (1 2 3 4 5 5 12)
