;; reprezentare convențională
;; pentru True și False

(define true
  (lambda (x)
    (lambda (y)
      x)))

(define false
  (lambda (x)
    (lambda (y)
      y)))

(define and
  (lambda (x)
    (lambda (y)