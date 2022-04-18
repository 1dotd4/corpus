#!/usr/local/bin/chicken-csi -script
(import 
  (chicken io)
  (chicken string)
  (chicken process)
  (chicken process-context)
  ;
  )

(define (read-pdf filename)
  (with-input-from-pipe
    (conc "pdftotext "
          filename
          " -")
    (lambda ()
      (read-string #f))))

(define (split-pages s)
  (string-split s "" #t))

(define (split-lines s)
  (string-split s "\n" #t))

(define (split-words s)
  (string-split s " "))

(define (init-database) '())

; (define (insert-term doc-name
;   (lambda (term db)
;   (let ((document-list (db->document-list db))
;         (current-term-count-documents (db->term-document-count db))
;         (current-document-term-count (db->term-document-count doc-name term)))
;     (db:merge db
;               (insert-document-list db document-list doc-name)
;               (insert-term-count-document db current-term-count-documents 
;   )

(define (insert-term ts t)
  (cons
    (add1 (car ts))
    (alist-update
      t
      (add1 (alist-ref t (cdr ts) string=? 0))
      (cdr ts)
      string=?)))

(define (insert-document doc-name db)
  (let ((terms 
          ((o ; filter / map here
              split-words)
           (read-pdf doc-name))))
    (cons (cons doc-name (fold insert-term '(0 . ()) terms)) db)))

(define (document->name d) (car d))

(define (document-similarity>? a b)
  (> (cadr a) (cadr b)))


;; vector operations

(define (dot-product a b)
  (fold + 0 (map (lambda (z) (* (car z) (cadr z))) (zip a b))))

(define (norm-2 a)
  (sqrt (fold + 0 (map (lambda (x) (* x x)) a))))

(define (similarity q)
  (lambda (dj)
    (/ (dot-product dj q)
       (* (norm-2 dj)
          (norm-2 q)))))

;; search

(define (search query db)
  (let* ((query-terms  (split-words query))
         (query-vector (map (lambda (x) 1) (iota (length query-terms))))
         (terms-vectors (document-vectors query-vector db)))
    (sort 
      (zip (map document->name db)
           (map (similarity query-vector)
                terms-vectors))
      document-similarity>?)))


(search "elliptic curve"
  (fold
    insert-document
    (init-database)
    (command-line-arguments)))

