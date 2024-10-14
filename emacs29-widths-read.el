;; -*- lexical-binding: t -*-
(require 'emacsql-sqlite-builtin)
(require 'emacsql-sqlite)
(require 'emacsql)
;; requires emacs29
(defvar db (emacsql-sqlite-builtin "widths.db"))
;; `object-slots'

;; requires debian10/sqlite3-pcre package
(if-let ((dbptr (eieio-oref db 'handle)))
    (let ()
       ;; in a emacsql-sqlite handle, this will fail sqlite-p test that is used in
       ;; sqlite-load-extension
       (sqlite-load-extension dbptr "/usr/lib/sqlite3/pcre.so")

       (emacsql db
                [:select [width code-a code-b]
                 :from widths
                 ;; - REGEXP is usually unimplemented in sqlite3
                 ;; - symbols compile as strings in emacsql "[a-z]" works aswell
                 :where code-b :regexp '[a-z]]))
  (error "DB is not found or initialised properly!"))
