;;;; corpus - indexing and searching PDFs.
;;
;; Copyright (C) 2022 d4 (unpx.net)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(import 
  (srfi-1)
  (srfi-13)
  (srfi-28)
  (args)
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
  (chicken process signal)
  ;
  spiffy
  intarweb
  uri-common
  sxml-serializer
  )

;; Version of the software
(define +version+ "corpus 0.3")
 ;; Selected server port
(define *selected-server-port* 6660)

(define +style+ "
* { padding: 0; margin: 0; box-sizing: border-box; }

body { font-family: sans-serif; font-size: 18px; }
h1, h2, h3, h4, h5, h6, p { margin-bottom: 1rem; }
small { font-weight: lighter; }

.content { width: 100%; }

.nav-wrap { background-color: #c6e6f8; border-bottom: solid 1px #c6e6f8; }
.nav { display: flex; justify-content: space-between; }
.nav * { display: inline; }
.nav input { font-size: 1.3em; line-height: 1.3;  padding: 0.2em 0.5em; border-radius: 0; border: none; }
.search { width: 90%; }

input[type=\"submit\"] { background-color: #c6e6f8; }
input[type=\"text\"] { background-color: #FFFFFF; }

.title:before {
  font-family: serif;
  display: inline-block;
  padding: 0.2rem;
  margin-bottom: 0.1rem;
  margin-right: 0.3rem;
  content: \"c.\";
  font-weight: bold;
  font-size: 2em;
}

dl { margin: 1rem 0; }
dl div { padding: 0.8rem 0; border-bottom: solid 1px #c6e6f8; }
dl dt { margin-bottom: 0.5rem; }
dl dd { font-size: 0.9rem; margin: 0.1rem 0.7rem; font-weight: lighter; }

p { margin-top: 1em; }

@media only screen and (min-width: 401px) and (max-width: 960px) {
  .content { width: 90%; margin: 0 auto; }
}
@media only screen and (min-width: 961px) {
  body { font-size: 20px; }
  .content { width: 960px; margin: 0 auto; }
  .title::before { content: attr(title); }
}
")

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

(define (display-results results)
  (map (lambda (r) (format #t "~5F\t~A\n" (cadr r) (car r))) ; TODO: better methods for result inspection
        (reverse results)))
;; Search
(define (search-from-sexp operands)
  (display-results
    (db:search
      (db:load-database)
      (string-join operands " "))))


;;; Web server

(define (web:database) '())
(define (reload-database n)
  (print n)
  (set! web:database (db:load-database))
  (print "Reloaded!"))

(define (render-results results)
  `(
    (small (p (conc "Found " ,(number->string (length results)) " documents.")))
    (dl
      ,(map
         (lambda (r)
           `(div
              (dt ,(car r))
              (dd ,(cadr r))))
         results))
    ))

(define +not-found-page+
  `(html
     (head
       (meta (@ (charset "utf-8")))
       (title "Page not found - corpus")
       (meta (@ (name "viewport") (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
       (meta (@ (name "author") (content "d4")))
       (style ,+style+))
     (body (h1 "Not found!"))))

(define +home-page+
  `(div (@ (class "banner"))
        (p "Welcome to corpus. Use the search bar to query for terms.")))

(define (build-page title content)
  ;; Function that build the appropriate page
  `(html
     (head
       (meta (@ (charset "utf-8")))
       (title ,(string-append title " - corpus"))
       (meta (@ (name "viewport") (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
       (meta (@ (name "author") (content "d4")))
       (style ,+style+))

     (body
       (div (@ (class "nav-wrap"))
            (form (@ (class "content nav")
                     (action "#")
                     (method "GET"))
                  (label (@ (class "title")
                            (title "corpus"))
                         "")
                  (input (@ (class "search")
                            (name "s")
                            (placeholder "Type here some terms to search")))
                  (input (@ (type "submit")
                            (value "Go")))))
       (div (@ (class "content"))
            ,content
            (p
              (small
                ,(conc "Loaded in " "2s" " · ")
                (a (@ (class "text-info")
                      (href "http://unpx.net/code/git-overview.git/"))
                   ,+version+)))))))

(define (send-sxml-response sxml)
  ;; Function to serialize and send SXML as HTML
  (with-headers
    `((connection close))
    (lambda () (write-logged-response)))
  (serialize-sxml
    sxml
    output: (response-port (current-response)))) 

(define (handle-request continue)
  ;; Function that handles an HTTP requsest in spiffy
  (let* ((uri (request-uri (current-request)))
         (path (uri-path uri))
         (current-time (current-seconds))
         (query (uri-query uri))
         (search-string (assoc 's query)))
    (cond ((equal? path '(/ "corpus" "")) 
           (send-sxml-response
             (if (and search-string (not (string=? "" (cdr search-string))))
               (build-page (cdr search-string)
                           (render-results (db:search (db:load-database) (cdr search-string))))
               (build-page "Home" +home-page+))))
          (else (send-sxml-response +not-found-page+)))))

 ;; Map a any vhost to the main handler
(vhost-map `((".*" . ,handle-request)))

(define (serve)
  (set-signal-handler! signal/hup reload-database)
  (reload-database 0)
  (server-port *selected-server-port*) 
  (start-server))

;; Options handling
(define operation 'none)
(define opts
  ;; List used by args:parse to choose which option will be selected
  (list (args:make-option
          (a add) #:none "Add document to the index"
          (set! operation 'add))
        (args:make-option
          (s serve) #:none "Serve current database"
          (set! operation 'serve))
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
        ((equal? operation 'serve)
         (print (conc "Will start web server at port " *selected-server-port*))
         (serve))
        (else
          (print (conc "Searching for terms: `" (string-join operands " ") "`."))
          (search-from-sexp operands))))

;; TODO: print only if asked to
(print-cpu-time)
