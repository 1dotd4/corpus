#!/usr/local/bin/chicken-csi -script
(import 
  (srfi-1)
  (srfi-13)
  (chicken io)
  (chicken sort)
  (chicken time)
  (chicken time posix)
  (chicken format)
  (chicken string)
  (chicken process)
  (chicken process-context)
  ;
  )

(define (split-words s)
  (string-split s " \n.,"))

(define (document->name d) (car d))

(define (document-vector-similarity>? a b)
  (> (cadr a) (cadr b)))

;; vector operations

(define (dot-product a b)
  (fold + 0 (map (lambda (z) (* (car z) (cadr z))) (zip a b))))

(define (norm-2 a)
  (sqrt (fold + 0 (map (lambda (x) (* x x)) a))))

(define (similarity q)
  (lambda (dj)
    (print dj)
    (/ (dot-product dj q)
       (* (norm-2 dj)
          (norm-2 q)))))

;; search

(define (document-vectors query-terms db)
  (define (count-document-matched t)
    (length
      (filter 
        (lambda (d) (alist-ref t (cddr d)))
        db)))
  (let ((no-docs (length db)))
    (define (doc-vect doc)
      (map
        (lambda (t)
          (let* ((term-doc-match (count-document-matched t))
                 (idf (/ (log (/ no-docs (add1 term-doc-match)))
                         (log 10))))
            (print (car doc))
            (print (length (cddr doc)))
            ; (print (cddr doc))
            (print (alist-ref t (cddr doc) string=? 0))
            (print (cadr doc))
            (print idf)
            (* (/ (alist-ref t (cddr doc) string=? 0)
                  (cadr doc))
               idf)))
        query-terms))
    (map doc-vect db)))


(define (search query db)
  (print (cpu-time))
  (print "Searching...")
  (let* ((query-terms  (split-words query))
         (query-vector (map (lambda (x) 1) (iota (length query-terms))))
         (terms-vectors (document-vectors query-terms db)))
    (sort 
      (filter
        (lambda (d) (> (cadr d) 0))
        (zip (map document->name db)
             (map 
               (similarity query-vector)
               terms-vectors)))
      document-vector-similarity>?)))

(define (load-database)
  (with-input-from-file
    "database.sexp"
    (lambda () (read))))

(let* ((args (command-line-arguments)))
  (print
    (search
      (string-join (command-line-arguments) " ")
      (load-database))))

(print (cpu-time))
