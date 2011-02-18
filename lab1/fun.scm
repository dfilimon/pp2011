;; Scheme!
; totul e o listă
; (+ 1 2) -> 3
; (length '(1 2 3)) -> 3
; (max 3 4) -> 4
; '(1 2 3) = listă 1 2 3
; (1 2 3) = aplică funcția 1 cu parametrii 2 3
; ' are rolul de a _împiedica evaluarea interiorului_

; '(+ (+ 1 2) 3) = e un șir!
; () = lista vidă

; (cons 2 '(3 4)) -> listă nouă '(2 3 4)
; (car l) = 2
; (cdr l) = (3 4)

; listele pot fi heterogene
; '(2 + #t "Hello")

; (null? l) -> dacă lista l este gaolă

; reprezentarea cu perechi...
; (1 2 3) <-> (1 . (2 . (3 . ())))
; listele _în spate_ sunt reprezentate așa

; funcții în Scheme -> calcul lambda
; (lambda(x)
;  x)
; \x . x

; leg variabila idd la funcția anonimă f(x) = x
; putem lucra cu funcții anonime!
(define idd
  (lambda (x)
    x
    )
  )

; ce întoarce...
;1 ]=> idd
;Value 17: #[compound-procedure 17 idd]

; se poate primi pe sine ca parametru:
; 1 ]=> (idd idd)
;Value 17: #[compound-procedure 17 idd]

(define ct
  (lambda ()
    3
    )
  )

;; 1 ]=> (ct)

;; ;Value: 3

;; 1 ]=> ct

;; ;Value 18: #[compound-procedure 18 ct]

; suma între 2 numere

; (sum1 1 2) - funcție de 2 parametri
(define sum1
  (lambda (x y)
    (+ x y)
    )
  )

; ((sum2 1) 2) - evaluez (sum2 1) care e o funcție de 1 parametru, decid cine e primul
; parametru, întoarce o funcție care primește următorul argument, y care are toți
; parametrii fixați și poate calcula suma
; (sum2 1) - întoarce o funcție
(define sum2
  (lambda (x) ; (sum2 1) este funcția care începe aici ---
    (lambda (y)
      (+ x y)
      )
    ) ; și se termină aici ---
  )

; funcție compose, care generează
; (compose (f g)) = f . g
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x))
      )
    )
  )

; 2 error> ((compose (sum2 1) (sum2 2)) 3)
; Value: 6

; if este o funcție...
; (if <cond> <true_expr> <false_expr>)

; minimum unei liste, cu numere pozitive!
(define min_list
  (lambda (l)
    (if (null? l)
        #f
        (if (null? (cdr l))
            (car l)
            (min (car l) (min_list (cdr l)))
            )
        )
    )
  )

; verificarea tipurilor nu se face la compilare, ci la runtime
; => weakly-typed

; funcția întoarce primul element par, prin testare modulo 2
(define find_first_even
  (lambda (l)
    (if (eq? (modulo (car l) 2) 0)
        (car l)
        (find_first_even (cdr l))
    )
    )
  )
;6 error> (find_first_even '(3 #f 2 4))
;The object #f, passed as the first argument to integer-remainder, is not the correct type.

;6 error> (find_first_even '(3 2 4))
;Value: 2

; putem verifica programatic dacă anumite date sunt de diverse tipuri sau nu
; list?
; number?
; pair?
  