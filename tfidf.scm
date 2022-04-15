(import (srfi-1))

(define documents-terms
  '( ; documents
    ((this   . 1)
     (is     . 1)
     (a      . 2)
     (sample . 1))
    ((this    . 1)
     (is      . 1)
     (another . 2)
     (example . 3))
    ))

(define (wiki:search-term term documents-terms)
  (define (count-terms doc)
    (fold + 0 (map cdr doc)))
  (define (count-document-matches term docs)
    (length
      (filter
        (lambda (d) (alist-ref term d))
        docs)))
  (define (idf term docs)
    (/ (log
         (/ (length docs)
            (count-document-matches term docs)))
       (log 10)))
  (define (tf term doc)
    (/ (alist-ref term doc eqv? 0)
       (count-terms doc)))
  (let ((docs-idf (idf term documents-terms)))
    (map
      (lambda (d)
        (* (tf term d)
           docs-idf))
      documents-terms)))

(define (example)
  (print (wiki:search-term 'this documents-terms))
  (print (wiki:search-term 'example documents-terms)))

(example)
