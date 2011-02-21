; 1. Funcții
(define plus (lambda (x y) (+ x y)))

; 2. Funcții recursive
; - construcția soluțiilor pe revenire din recursivitate
(define factr
  (lambda (n)
    (if (= n 0) 1
        (* n (factr (- n 1)))
        )
    )
  )
           
; - construcția soluțiilor pe avans în recursivitate
(define factaux
  (lambda (n acc)
    (if (= n 1) acc
        (factaux (- n 1) (* acc n))
        )
    )
  )
(define facta
  (lambda (n)
    (factaux n 1)
    )
  )

; Tail recursion
; funcții care nu folosesc stiva pentru calcule
; facta - tail recursive function

; 1 ]=> (factr 100000)
;Aborting!: maximum recursion depth exceeded

; tutorial scheme
; http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-1.html#node_toc_start
; capitolele 3, 4 din tutorial
; 4: alte structuri în afară de if

; let, let*
; let - creează variabile locale
(define x 10)
; 10

(let ((x 1) (y 2)) ; listă perechi, (variabilă locală | valoare)
  (+ x y)) ; x aici nu mai e 10, va fi 3, umbrește 10 de mai înainte
; 3, un let e o valoare!

(let ((x 1) (y x)) ; x aici nu a fost încă definit, nu face toate legările imediat
  (+ x y)) ; corpul lui let
; 11, x va fi 11 când y e egal cu x

; let* e un let într-un let
; http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-7.html#node_sec_5.1
(let* ((x 1) (y x)) ; let* vede fiecare variabilă la inițializări
  (+ x y))
; 2

; letrec, named let
; letrec, doar pentru funcții
; ceea ce definesc se vede și în inițializări, și după și înainte și în
; corpul lui let
; (letrec ( (f1 (corp f1)) (f2 (corp f2)) ... ) (corp letrec))

; cum se poate scrie cu letrec, cum se prescurtează cu named let

; (letrec ( (f (lambda a b) (..calcul..)) )
;   (f 5 10))

; (let f ((a 5) (b 10)) (..calcul..))
; f va fi un nume de funcție (_named_ let)

; de rezolvat:
; 0. testați variantele factorialului
; 1. calculați lungimea unei liste pe avans și pe revenire în recursivitate
; 2. (1 2 3 4 . 5) -> (1 2 3 4 5) trebuie scos punctul
; 3. (1 2 3 4 5) -> (1 2 3 4 . 5), capitolul 2
; 4. scrieți o funcție care să calculeze lista pozițiilor aparițiilor unui element
; într-o listă: (1 2 1 2 3 1), caut 1 => (0 2 5), și pe avans și pe revenire
; 5. se dau n1, n2 \in N*, n1 <= n2, funcție care să întoarcă lista numerelor
; naturale ordonate crescător, cu letrec și named let, folosind countdown din tut
; 6. să se rescrie list-position din 6.3 folosind letrec
; 7. scrieți o funcție care să calculeze lungimea unei liste pe avans în
; recursivitate astfel: cu letrec / namedlet (cea locală să fie imbricată)

(define lengthr
  (lambda (l)
    (if (null? l) 0
        (+ 1 (lengthr (cdr l)))
        )
    )
  )

(define lengthaux
  (lambda (l acc)
    (if (null? l) acc
        (lengthaux (cdr l) (+ 1 acc))
        )
    )
  )
        
(define lengtha
  (lambda (l)
    (lengthaux l 0)
    )
  )

; (cons 1 (cons 2 3)) -> 1 2 3
(define dotless
  (lambda (l)
    (if (not (pair? (cdr l))) (cons (car l) (cons (cdr l) '()))
        (cons (car l) (dotless (cdr l)))
        )
    )
  )

; (1 2 3) -> (cons 1 (cons 2 3))
(define dotfull
  (lambda (l)
    (if (null? (cddr l)) (cons (car l) (cadr l))
        (cons (car l) (dotfull (cdr l)))
        )
    )
  )

(define dotfull2
  (lambda (l)
    (if (null? (cdr l)) (car l)
        (cons (car l) (dotfull2 (cdr l)))
        )
    )
  )

; liste: equal?
; numere: =
; general: eqv? (equivalent?)

(define findall
  (lambda (l x)
    (findallaux l x '() 0)
    )
  )

(define findallaux
  (lambda (l x acc pos)
    (if (null? l) acc
        (findallaux (cdr l) x
                    (if (= (car l) x) (cons pos acc)
                        acc)
                    (+ pos 1)
                    )
        )
    )
  )

(define findall2
  (lambda (l x)
    (findallaux2 l x 0)
    )
  )

(define findallaux2
  (lambda (l x pos)
    (if (null? l) '()
        (let ( (rest (findallaux2 (cdr l) x (+ pos 1))) )
          (if (= (car l) x) (cons pos rest)
              rest
              )
          )
        )
    )
  )

(define range
  (lambda (n1 n2)
    (letrec ( (countup
               (lambda (i)
                 (if (= i (+ n2 1)) '()
                     (cons i (countup (+ i 1)))
                     )
                 )
               ) ) 
      (countup n1)
      )
    )
  )

(define range2
  (lambda (n1 n2)
    (let countup ((i n1))
      (if (= i (+ n2 1)) '()
          (cons i (countup (+ 1 i)))
          )
      )
    )
  )

;; (define list-position
;;   (lambda (o l)
;;     (let loop ((i 0) (l l))
;;       (if (null? l) #f
;;           (if (eqv? (car l) o) i
;;               (loop (+ i 1) (cdr l)))))))

(define list-position
  (lambda (o l)
    (letrec ( (loop
               (lambda (i l)
                 (if (null? l) #f
                     (if (eqv? (car l) o) i
                         (loop (+ i 1) (cdr l))
                         )
                     )                 
                 )
               ) )
      (loop 0 l)
      )
    )
  )

(define lengthrec
  (lambda (l)
    (let lengthaux ((l l) (acc 0))
      (if (null? l) acc
          (lengthaux (cdr l) (+ 1 acc))
          )
      )
    )
  )

; list-position: să întoarcă lista cu toate aparițiile
(define list-positionrec
  (lambda (o l)
    (let loop ((i 0) (l l) (acc '()))
      (if (null? l) acc
          (loop (+ i 1) (cdr l)
                (if (eqv? (car l) o) (cons i acc)
                    acc)
                )
          )
      )
    )
  )