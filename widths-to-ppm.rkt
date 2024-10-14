#!/usr/bin/env racket
#lang racket
(require srfi/43)
(require srfi/1)
(require json)

(define (debugln datum)
  (display (format "~a~%" datum)
           (current-output-port)))

(define debug? (make-parameter #f))

(define (widths-to-line widths)
  (let ((wdthstr (foldr string-append ""
                        (map number->string widths))))
    (when (debug?) (debugln (format "widthstr: ~a" wdthstr)))
    (foldr string-append ""
           (foldr append (list)
                  (vector->list
                   (vector-map
                    (lambda (i x)
                      (let ((color
                             (if (= (modulo i 2) 0)
                                 "1"
                                 "0")))
                        (map (lambda _ color)
                             (iota (string->number (string x))))))
                    (list->vector (string->list wdthstr))))))))

(define AMOUNT-OF-LINES 50)
(define PAD 8)
(define (get-header line)
  (format "P1~%~a ~a~%"
          ;; pad
          (+ PAD (string-length line))
          (+ PAD AMOUNT-OF-LINES)))
(define (widths-to-png widths)
  (let* ((line (widths-to-line widths))
         (header (get-header line))
         (pad
          (map (lambda _
                 (format "~a~%"
                         (foldr string-append ""
                                (map (lambda _ "0")
                                     (iota (+ PAD (string-length line)))))))
               (iota (/ PAD 2))))
         (lines (foldr string-append ""
                       (append
                        (list header)
                        pad
                        (map (lambda _ (format "0000~a0000~%" line))
                             (iota AMOUNT-OF-LINES))
                        pad))))
    (display lines)))

(define (main json)
  (let* ((obj (with-input-from-string json
                (lambda () (read-json))))
         ;; barcode_widths are already passed in
         (widths (if (list? obj)
                     obj
                     ;; hash-ref with json works on symbols and not strings
                     (hash-ref obj 'barcode_widths))))
      (widths-to-png widths)))

(apply main (let ((args (vector->list (current-command-line-arguments))))
              (if (= 0 (length args))
                  (port->lines (current-input-port))
                  args)))
