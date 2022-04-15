(import (srfi-1))

(define (dot-product a b)
  (fold + 0 (map (lambda (z) (* (car z) (cadr z))) (zip a b))))

(define (norm-2 a)
  (sqrt (fold + 0 (map (lambda (x) (* x x)) a))))

(define (cosine dj q)
  (/ (dot-product dj q)
     (* (norm-2 dj)
        (norm-2 q))))

(define (example)
  (define q '(2 2))
  (define djs
    '((2 3)
      (5 2)))
  (print (map (lambda (dj) (cosine q dj))
              djs)))

(example)
