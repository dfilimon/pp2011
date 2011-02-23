; curried eq?
(define eq_?
  (lambda (x)
    (lambda (y)
      (eq? x y)
      )
    )
  )

; prefix?, to check for *shell* prefix
; check if p is a prefix of l
(define prefix?
  (lambda (p l)
    (cond ((null? p) #t) ; prefix is empty, we have succeeded
	  ((null? l) #f) ; l is empty, p is not empty, fail
	  ((eq? (car p) (car l)) ; current chars equal
	   (prefix? (cdr p) (cdr l))) ; continue with the rest of the lists
	  (else #f) ; current chars different, p is not a prefix
	  )
    )
  )

; check if s1 is a prefix of s2
; check for prefixes where s1 and s2 are _strings_
(define string-prefix?
  (lambda (s1 s2)
    (string=? s1 (substring s2 0 (string-length s1)))
    )
  )

; curry simple 2-argument function
;; f : (a x b) -> c => f : a -> (b -> c)
; transform a 2 argument function into a function that takes
; 2 arguments _sequentially_
(define curry2
  (lambda (f)
    (lambda (x)
      (lambda (y)
	(f x y)
	)
      )
    )
  )

;; 2 error> l
;; ;Value 2: (1 2 3 4 5 6 7)

; the partial function ((curry2 <) 4) = f
; evaluates (f x) = 4 < x
; so, 4 < 5 = #t => (((curry2 <) 4) 5) = #t!

; it is NOT a predicate that gets everything less than 4
; although, this is what I thought I had written at first!

;; 1 ]=> (map ((curry2 <) 4) l)
;; ;Value 3: (#f #f #f #f #t #t #t)

;; 1 ]=> (filter ((curry2 <) 4) l)
;; ;Value 4: (5 6 7)

; Also:
;; 1 ]=> (map ((curry2 eq?) 4) l)
;; ;Value 5: (#f #f #f #t #f #f #f)

;; 1 ]=> (map (eq_? 4) l)
;; ;Value 6: (#f #f #f #t #f #f #f)

; Objective:
; to kill all buffers that don't start with *shell*
; (map kill-buffer (filter ((curry2 prefix?) '*shell*) list-buffers))


