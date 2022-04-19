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
  (sql-de-lite)
  )

(define (read-pdf filename)
  (with-input-from-pipe
    (conc "pdftotext '"
          filename
          "' -")
    (lambda ()
      (read-string #f))))

(define (split-pages s)
  (string-split s "" #t))

(define (split-lines s)
  (string-split s "\n" #t))

(define (split-words s)
  (string-split s " \n.,;()[]{}'\""))

(define (init-database) '())

(define (insert-term t ts)
  (cons
    (add1 (car ts))
    (alist-update
      t
      (add1 (alist-ref t (cdr ts) string=? 0))
      (cdr ts)
      string=?)))

(define (insert-document doc-name db)
  ;(print (cpu-time))
  ;(print (format "Inserting ~A" doc-name))
  (let ((terms 
          ((o ; filter / map here
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
          insert-document
          (init-database)
          (command-line-arguments))))))

(define (init-sqlite-database db)
  (exec (sql db "create table if not exists documents (name text);"))
  (exec (sql db "create table if not exists termcount (docid INT, term TEXT, count INT DEFAULT 0, primary key (docid, term));"))
  (exec (sql db "create index terms on termcount (docid, term);")))

(define (insert-term-sqlite db docid)
  (lambda (term)
    (exec
      (sql db "insert or ignore into termcount (docid, term, count) values (?, ?, 0);")
      docid
      term)
    (exec
      (sql db "update termcount set count = count + 1 where docid = ? and term = ? ;")
      docid
      term)))

(define (insert-document-sqlite doc-name db)
  (exec (sql db "insert into documents (name) values (?) ;") doc-name)
  (let ((docid (exec (sql db "select last_insert_rowid();")))
        (terms 
          ((o ; filter / map here
             (cut map string-downcase <>)
             split-words)
           (read-pdf doc-name))))
    (print (cpu-time))
    (print docid)
    (map
      (insert-term-sqlite db (car docid))
      terms)
    db))




(define (index-to-sqlite)
  (call-with-database
    "database.sqlite3"
    (lambda (db)
      (begin
        (init-sqlite-database db)
        (fold
          insert-document-sqlite
          db
          (command-line-arguments))))))

(index-to-sqlite)

(print (cpu-time))
