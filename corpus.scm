#!/usr/bin/csi -script
(import 
  (srfi-1)
  (srfi-13)
  (args)
  (chicken io)
  (chicken port)
  (chicken sort)
  (chicken time)
  (chicken time posix)
  (chicken format)
  (chicken string)
  (chicken process)
  (chicken process-context)
  ;
  )

(define +version+ "corpus 0.1")

;; Helpers
(define (split-pages s)
  (string-split s "" #t))
(define (split-lines s)
  (string-split s "\n" #t))
(define (split-words s)
  (string-split s " \n.,;()[]{}'\""))

;; vector operations
(define (vector:similarity q)
  (define (dot-product a b)
    (fold + 0 (map (lambda (z) (* (car z) (cadr z))) (zip a b))))
  (define (norm-2 a)
    (sqrt (fold + 0 (map (lambda (x) (* x x)) a))))
  ;; "Higher order" function that calculate cosine of angle between vectors
  (lambda (dj)
    ; (print dj)
    (/ (dot-product dj q)
       (* (norm-2 dj)
          (norm-2 q)))))

;; Read PDF stuff
(define (read-pdf filename)
  (with-input-from-pipe
    (conc "pdftotext '"
          filename
          "' -")
    (lambda ()
      (read-string #f))))

;; Database operations
(define (db:load-database)
  (condition-case
    (with-input-from-file
      "database.sexp"
      (lambda () (read)))
    [(exn) '()]))
(define (db:insert-document doc-name db)
  (define (insert-term t ts)
    (cons
      (add1 (car ts))
      (alist-update
        t
        (add1 (alist-ref t (cdr ts) string=? 0))
        (cdr ts)
        string=?)))
  ;(print (cpu-time))
  ;(print (format "Inserting ~A" doc-name))
  ; TODO: find another way to print this without polluting the database
  (let ((terms 
          ((o ; it is possible to filter / map here
              (cut map string-downcase <>)
              split-words)
           (read-pdf doc-name))))
    (cons (cons doc-name (fold insert-term '(0 . ()) terms)) db)))
(define (db:document-vectors query-terms db)
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
(define (db:search db query)
  (print (cpu-time))
  (print "Searching...")
  (let* ((query-terms  (split-words query))
         (query-vector (map (lambda (x) 1) (iota (length query-terms))))
         (terms-vectors (db:document-vectors query-terms db)))
    (sort 
      (filter
        (lambda (d) (> (cadr d) 0))
        (zip (map document->name db)
             (map (vector:similarity query-vector)
                  terms-vectors)))
      document->greater)))

;; Document methods
(define (document->name d) (car d))
(define (document->greater a b)
  (> (cadr a) (cadr b)))

;;; Operations
;; Index
(define (index-to-sexp operands)
  (let ((db (fold
              db:insert-document
              (db:load-database)
              operands)))
    (with-output-to-file
      "database.sexp"
      (lambda () (write db)))))
;; Search
(define (search-from-sexp operands)
  (print
    (db:search
      (db:load-database)
      (string-join operands " "))))

;; Options handling
(define operation 'none)
(define opts
  ;; List used by args:parse to choose which option will be selected
  (list (args:make-option
          (a add) #:none "Add document to the index"
          (set! operation 'add))
        (args:make-option
          (v V version) #:none "Display version"
          (print +version+)
          (exit))
        (args:make-option (h help) #:none "Display this text" (usage))))
(define (usage)
  (with-output-to-port
    (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " [options...] [files...]")
      (newline)
      (print (args:usage opts))
      (print +version+)))
  (exit 1))

;; TODO: input line stuff
(receive
  ;; ...
  ; corpus -a document.pdf ...
  ; corpus "search query"
  (options operands)
  (args:parse (command-line-arguments) opts)
  (cond ((equal? operation 'add)
         (print "Will import `" (string-join operands " ") "`.")
         (print operands)
         (index-to-sexp operands))
        (else
          (print "Searching for terms: `" (string-join operands " ") "`.")
          (search-from-sexp operands))))


;; TODO: print only if asked to
(print (cpu-time))
