(import 
  (srfi-1)
  (srfi-13)
  (args)
  (format)
  (chicken io)
  (chicken port)
  (chicken sort)
  (chicken time)
  (chicken time posix)
  ; (chicken flonum) format already does that
  ;(chicken format) smaller lib we don't need
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
(define +cpu-stop+ 0)
(define (set-cpu-time!)
  (receive
    (a b)
    (cpu-time)
    (set! +cpu-stop+ a)))
(define (print-cpu-time)
  (receive
    (a b)
    (cpu-time)
    (format #t "~Ams\n" (- a +cpu-stop+))))


;; vector operations
(define (vector:similarity q)
  (define (dot-product a b)
    (fold + 0 (map (lambda (z) (* (car z) (cadr z))) (zip a b))))
  (define (norm-2 a)
    (sqrt (fold + 0 (map (lambda (x) (* x x)) a))))
  ;; "Higher order" function that calculate cosine of angle between vectors
  (lambda (dj)
    ; (print dj)
    (let ((norm-product 
            (* (norm-2 dj)
               (norm-2 q))))
      (if (zero? norm-product)
        0
        (/ (dot-product dj q)
           norm-product)))))

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
  ;;;; Computation overview
  ;; query-terms: (q1 q2 ... qn)
  ;;; 1. compute now the number of documents that match a given term
  ;; ((q1 . m1) (q2 . m2) ... ).
  ;;; 2. for each document with this new term list compute the vector
  ;; ((doc1 ... (v1 v2 ... vn)) ...)
  ;;; 3. compute the score based on the query vector
  (define (count-document-matched t)
    (length
      (filter 
        (lambda (d) (alist-ref t (cddr d) string=?))
        db)))
  (define (doc:contains-terms ts)
    (lambda (tf) (member (car tf) ts string=?)))
  (let* ((no-docs (length db))
         ;; 1
         (terms-doc-match (zip query-terms
                               (map count-document-matched
                                    query-terms))))
    (define (doc-vect doc)
      (let ((smaller-doc (filter (doc:contains-terms query-terms)
                                 (cddr doc))))
        (map
          (lambda (t)
            (let* ((term-doc-match (cadr t))
                   (idf (/ (log (/ no-docs (add1 term-doc-match)))
                           (log 10))))
              ; (print (car doc))
              ; (print (length (cddr doc)))
              ; ; (print (cddr doc))
              ; (print (alist-ref t (cddr doc) string=? 0))
              ; (print (cadr doc))
              ; (print idf)
              (if (zero? (cadr doc))
                0
                ;; 2,3
                (* (/ (alist-ref (car t) smaller-doc string=? 0)
                      (cadr doc))
                   idf))))
          terms-doc-match)))
    (map doc-vect db)))
(define (db:search db query)
  (set-cpu-time!)
  (print "Searching...")
  (let* ((query-terms  (split-words query))
         ; todo: maybe compute tfidf for query too
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
  (define (display-results results)
    (map (lambda (r) (format #t "~5F\t~A\n" (cadr r) (car r))) ; TODO: better methods for result inspection
         (reverse results)))
  (display-results
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
      (print (conc "Usage: " (car (argv)) " [options...] [files...]"))
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
         (print (conc "Will import `" (string-join operands " ") "`."))
         (index-to-sexp operands))
        (else
          (print (conc "Searching for terms: `" (string-join operands " ") "`."))
          (search-from-sexp operands))))

;; TODO: print only if asked to
(print-cpu-time)
