;; -*- lexical-binding: t -*-
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'emacsql-sqlite-builtin)
(require 'cl-lib)
(require 'dash)
(require 'json)

(defvar make-widths/widths
  (with-temp-buffer
    (shell-command
     "racket -e '(begin (require \"barcode.rkt\") (dump-state true))'" (current-buffer))
    (point-max)
    (cdar (json-read)))
 "program dump from `barcode.rkt'")

(defvar db (emacsql-sqlite-builtin "widths.db"))
(unless (-elem-index '(widths) (emacsql-sqlite-list-tables db))
  (emacsql db
           [:create-table widths
            ([(width :primary-key)
              code-a code-b code-c])]))

(unless (emacsql db [:select :* :from widths])
  (mapc
   (lambda (x)
     (emacsql db
              [:insert-into widths :values $v1]
              `(,(vector (alist-get :width  x)
                         (alist-get :code-a x)
                         (alist-get :code-b x)
                         (alist-get :code-c x)))))
   make-widths/widths))
;(emacsql db [:select [width code-a] :from widths])
