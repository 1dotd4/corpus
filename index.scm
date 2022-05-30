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

;; Helpers
(define (split-pages s)
  (string-split s "" #t))
(define (split-lines s)
  (string-split s "\n" #t))
(define (split-words s)
  (string-split s " \n.,;()[]{}'\""))

;; Read PDF stuff
(define (read-pdf filename)
  (with-input-from-pipe
    (conc "pdftotext '"
          filename
          "' -")
    (lambda ()
      (read-string #f))))

;; Database operations
(define (db:init-database) '())
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
  (print (format "Inserting ~A" doc-name))
  (let ((terms 
          ((o ; it is possible to filter / map here
              (cut map string-downcase <>)
              split-words)
           (read-pdf doc-name))))
    (cons (cons doc-name (fold insert-term '(0 . ()) terms)) db)))

(define (index-to-sexp)
  (with-output-to-file
    "database.sexp"
    (lambda ()
      (write
        (fold
          db:insert-document
          (db:init-database)
          (command-line-arguments))))))

;; TODO: input line stuff

(index-to-sexp)

(print (cpu-time))
