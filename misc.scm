(define (replace l i x)
  (if (= i 0) (cons x (cdr l))
      (cons (car l) (replace (cdr l) (- i 1) x))))
      
(define (filter2 l p)
  (if (null? l) l
      (let ((rest (filter2 (cdr l) p)))
        (if (p (car l)) (cons (car l) rest)
            rest))))

(define (quicksort2 l)
  (if (null? l) l
      (let ((less (filter2 l (lambda (x) (< x (car l)))))
            (equal (filter2 l (lambda (x) (= x (car l)))))
            (greater (filter2 l (lambda (x) (> x (car l))))))
        (append (quicksort2 less) equal (quicksort2 greater)))))