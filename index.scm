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

(print
  (car (split-pages
  (apply read-pdf (command-line-arguments)))))

