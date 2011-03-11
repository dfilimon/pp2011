;; ----- Booleans -----

(define true_0 (lambda (x y) x))
(define false_0 (lambda (x y) y))

;;(define not_0 ...)
(define not_0
  (lambda (x) (x false_0 true_0)))

;;(define and_0 ...) ; true and _ = _, false and _ = false
(define and_0
  (lambda (x y)
    (x y false_0)))

;(define or_0 ...) ; true or _ = true, false or _ = _
(define or_0
  (lambda (x y)
    (x true_0 y)))

;(define-macro if_0 ...)
(define-macro if_0
  (lambda (c t e)
    `((,c (lambda () ,t)
	 (lambda () ,e))) ))
;; ` (back quote), nu quote simplu
;; , înainte de un simbol înconjurat de ` înlocuiește simbolul
;; cu reprezentarea sa textual
;; (if_0 true (not true) (not false)

(and_0 true_0 true_0)
(or_0 true_0 false_0)

;; ----- Pairs -----

(define pair_0 (lambda (x y) (lambda (z) (z x y))))

;(define fst_0 ...) ; first pair element
(define fst_0
  (lambda (p)
    (p true_0)))

;(define snd_0 ...) ; second pair element
(define snd_0
  (lambda (p)
    (p false_0)))

;; ----- Lists -----

;(define cons_0 ...)
(define cons_0
  (lambda (x l)
    (pair_0 x l)))

;(define car_0 ...)
(define car_0
  (lambda (l)
    (fst_0 l)))

;(define cdr_0 ...)
(define cdr_0
  (lambda (l)
    (snd_0 l)))
(define nil_0 (lambda (z) true_0)) ; empty list
(define null_0 (lambda (l) (l (lambda (x y) false_0)))) ; empty list test

;; ----- Natural numbers -----

;(define zero_0 ...)
(define zero_0 nil_0)

;(define succ_0 ...)
(define succ_0 
  (lambda (l)
    (cons_0 succ_0 l)))

;(define pred_0 ...)
(define pred_0
  (lambda (l)
    (cdr_0 l)))

;(define zerop_0 ...)
(define zerop_0 null_0)

;; ----- Number convertors from Scheme to lambda_0 -----

; Converts numbers from lambda_0 representation to standard representaion (1, 4, 7 etc)
;(define lambda_0->std ...)
(define lambda_0->std
  (lambda (n)
    (if_0 (zerop_0 n) 0
        (+ 1 (lambda_0->std (pred_0 n))))))

; Converts numbers from standard representation to lambda_0 representaion
;(define std->lambda_0 ...)
(define std->lambda_0
  (lambda (n)
    (if (= n 0) nil_0
        (cons_0 succ_0 (std->lambda_0 (- n 1))))))
(lambda_0->std (std->lambda_0 9))

;; ----- List convertor from lambda_0 to Scheme -----

;(define lst ...)
(define lst
  (lambda (l)
    (if_0 (null_0 l) '()
          (cons (car_0 l) (lst (cdr_0 l))))))

(define l0 (lst (cons_0 (std->lambda_0 1) (cons_0 (std->lambda_0 2) nil_0))))
l0
(map lambda_0->std l0)

;; ----- Recursive functions for lists and numbers -----

; -- append the two lists --

;(define append_0 ...)
; concantenez lista l2 la lista l1
(define append_0
  (lambda (l1 l2)
    (if_0 (null_0 l1) l2
          (cons_0 (car_0 l1) (append_0 (cdr_0 l1) l2)))))
          
(map lambda_0->std
 (lst (append_0 (cons_0 (std->lambda_0 1) (cons_0 (std->lambda_0 2) (cons_0 (std->lambda_0 3) nil_0))) (cons_0 (std->lambda_0 4) (cons_0 (std->lambda_0 5) (cons_0 (std->lambda_0 6) nil_0))))))

; -- reverse list --

;(define reverse_0 ...)
(define reverse_0
  (lambda (l)
    (if_0 (null_0 l) nil_0
        (append_0 (reverse_0 (cdr_0 l)) (cons_0 (car_0 l) nil_0) ))))

(lst (reverse_0 (cons_0 1 (cons_0 2 (cons_0 3 (cons_0 4 nil_0))))))

; -- less than --

;(define lt_0 ...)
(define lt_0
  (lambda (x y)
    (if_0 (null_0 x) true_0
          (if_0 (null_0 y) false_0
              (lt_0 (cdr_0 x) (cdr_0 y))))))

(lt_0 (std->lambda_0 7) (std->lambda_0 8))

; -- equal --

;(define eq_0 ...)
(define eq_0
  (lambda (x y)
    (if_0 (and_0 (null_0 x) (null_0 y)) true_0
          (if_0 (or_0 (null_0 x) (null_0 y)) false_0
                (eq_0 (cdr_0 x) (cdr_0 y))))))
          

(eq_0 (std->lambda_0 9) (std->lambda_0 5))

; -- sum --

;(define +_0 ...)

; -- difference ---

;(define -_0 ...)

;(lambda_0->std (-_0 (std->lambda_0 5) (std->lambda_0 3)))

; -- multiplication ---

;(define *_0 ...)

;(lambda_0->std (*_0 (std->lambda_0 7) (std->lambda_0 8)))

; -- integer division --

;(define div_0 ...)

;(lambda_0->std (div_0 (std->lambda_0 100) (std->lambda_0 8)))

; -- modulo --

;(define mod_0 ...)

;(lambda_0->std (mod_0 (std->lambda_0 106) (std->lambda_0 8)))