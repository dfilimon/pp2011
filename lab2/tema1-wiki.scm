;; tema 1
;; http://elf.cs.pub.ro/pp/teme/t1

(define palindrome?
  (lambda (l)
    (equal? l (reverse l))
    )
  )

;; l = '(1 (2 3 (4) ((5 6 7) 8 9)))
;; (car l) : list
;; (car l): element
(define flatten
  (lambda (l)
    (if (null? l) l
	(let ( (tail (flatten (cdr l))) )
	  (if (list? (car l)) (append (flatten (car l)) tail)
	      (cons (car l) tail)
	      )
	  )
	)
    )
  )

(define curry2
  (lambda (f)
    (lambda (x)
      (lambda (y)
	(f x y)
	)
      )
    )
  )

(define power-set
  (lambda (l)
    (if (null? l) (list '())
	(let* ( (sub-power-set (power-set (cdr l)))
		(with-head (map ((curry2 cons) (car l)) sub-power-set)) )
	  (append with-head sub-power-set)
	  )
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
  (lambda (f l1 l2)
    (cond ((null? l1) l2)
	  ((null? l2) l1)
	  ((f (car l1) (car l2))
	   (cons (car l1) (merge f (cdr l1) l2))
	   )
	  (else
	   (cons (car l2) (merge f l1 (cdr l2)))
	   )
	  )
    )
  )

(define merge-sort
  (lambda (l f)
    (let ( (n (length l)) )
      (if (null? (cdr l)) l
	  (let* ( (n2 (quotient n 2))
		  (left (first-n l n2))
		  (right (last-n l n (- n n2))) )
	    (merge f (merge-sort left f) (merge-sort right f))
	    )
	  )
      )
    )
  )

;; l1 < l2 ?
;; în ipoteza în care au aceeași lungime!
(define lexicographic-less
  (lambda (l1 l2)
    (cond ((null? l1) #t)
          ( (= (car l1) (car l2))
	    (lexicographic-less (cdr l1) (cdr l2)) )
	  (else (< (car l1) (car l2))))
    )
  )

(define list-compare
  (lambda (l1 l2)
    (let ( (len1 (length l1))
	   (len2 (length l2)) )
      (if (= len1 len2) (lexicographic-less l1 l2)
	  (< len1 len2))
      )
    )
  )

;; cart-n
;; iau capul lui l, prima lista l1
;; il adaug la inceputul fiecarei liste
(define add-all
  (lambda (l lists)
    ()
    )
  )

(define cart-n
  (lambda (l)
    (if (null? l) '()
	(let ( (cart-n1 (cart-n (cdr l)))
	       (l1 (car l)) )
	  (add-all l cart-n1))
	)
    )
  )