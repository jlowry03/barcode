#!/usr/bin/env racket
#lang racket
(require json)
(require srfi/43)
;;; CODE-128 ENCODING
;; https://en.wikipedia.org/wiki/Code_128#Start/stop_and_encoded_data

(define (mk-code lst)
  `(,@lst START-CODE-A START-CODE-B START-CODE-C STOP REVERSE-STOP END))

(define (mk-ab-shared lst)
  `(#\space #\! #\" #\# #\$ #\% #\& #\‘ #\) #\( #\* #\+ #\, #\- #\. #\/ 
    ;; bug with #\& duplication
    ;; + #\Z missing
    ;; thus HWK -> IXL 
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\: #\; #\< #\= #\> #\? #\@

    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S
    #\T #\U #\V #\W #\X #\Y #\Z

    #\[ #\\ #\] #\^ #\_
    ,@lst FNC1))
(define code-a
  (mk-code
   (mk-ab-shared
    (list
     'NUL 'SOH 'STX 'ETX 'EOT 'ENQ 'ACK 'BEL 'BS 'HT 'LF 'VT 'FF 'CR 'SO 'SI
     'DLE 'DC1 'DC2 'DC3 'DC4 'NAK 'SYN 'ETB 'CAN 'EM 'SUB 'ESC 'FS 'GS 'RS 'US
     'FNC3 'FNC2 'SHIFT 'CODE-C 'CODE-B 'FNC4))))
(define code-b
  (mk-code
   (mk-ab-shared
    (list
     #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r
     #\s #\t #\u #\v #\w #\x #\y #\z #\{ #\| #\} #\~ 'DEL
     'FNC3 'FNC2 'SHIFT 'CODE-C 'FNC4 'CODE-A))))
(define code-c
  (mk-code
   (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
         27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
         51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
         75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
         99 'CODE-B 'CODE-A 'FNC1)))

;;https://en.wikipedia.org/wiki/Code_128#Bar_code_widths
;"(define widths '("+
; Array.from(
;     Array.from(
;        document.getElementsByClassName(
;          "wikitable sortable jquery-tablesorter"
;        )[0].childNodes
;    ).filter((x)=>x.nodeName==='TBODY')[0].children
;  ).map((x)=>x.cells[x.cells.length-1])
;   .filter(x=>x!==undefined)
;   .map(x=>x.innerText)
;   .toString().replaceAll(","," ") + "))"
(define widths
  '(212222 222122 222221 121223 121322 131222 122213 122312 132212 221213 221312
    231212 112232 122132 122231 113222 123122 123221 223211 221132 221231 213212
    223112 312131 311222 321122 321221 312212 322112 322211 212123 212321 232121
    111323 131123 131321 112313 132113 132311 211313 231113 231311 112133 112331
    132131 113123 113321 133121 313121 211331 231131 213113 213311 213131 311123
    311321 331121 312113 312311 332111 314111 221411 431111 111224 111422 121124
    121421 141122 141221 112214 112412 122114 122411 142112 142211 241211 221114
    413111 241112 134111 111242 121142 121241 114212 124112 124211 411212 421112
    421211 212141 214121 412121 111143 111341 131141 114113 114311 411113 411311
    113141 114131 311141 411131 211412 211214 211232 233111 211133 2331112))

;;; --------------------- DO NOT EDIT ABOVE THIS LINE ----------------------- ;;;
(define (debugln datum)
  (displayln datum (current-output-port)))

(define (dump-state lisp?)
  ;; DEBUGGER FUNCTION FOR WIDTHS, CODE-A, CODE-B, CODE-C
  (let* ((conv-code
          (lambda (y)
            (cond ((char? y)   (list->string (list y)))
                  ((symbol? y) (symbol->string y))
                  (else y))))
         (make-cons
          (lambda (w a b c)
            `(:width ,w
              :code-a ,(conv-code a)
              :code-b ,(conv-code b)
              :code-c ,(conv-code c))))
         (make-hash
             (lambda (w a b c)
               (apply hasheq
                   (make-cons w a b c)))))
    (write-json
     (hasheq ':widths
             (map make-hash widths code-a code-b code-c)))
    (newline)))
;;; https://github.com/racket/racket7/blob/5dbb62c6bbec198b4a790f1dc08fef0c45c2e32b/racket/collects/racket/list.rkt#L866
;;; only needed for racketscript
;;(define (index-where ls f)
;; (unless (list? ls)
;;   (raise-argument-error 'index-where "list?" 0 ls f))
;; (unless (and (procedure? f)
;;              (procedure-arity-includes? f 1))
;;   (raise-argument-error 'index-where "(-> any/c any/c)" 1 ls f))
;; (let loop ([ls ls]
;;            [i 0])
;;   (cond [(null? ls) #f]
;;         [(f (car ls)) i]
;;         [else (loop (cdr ls) (add1 i))])))

(define (chars->num str)
  ;; make it even length
  (cond ((symbol? str)
         (list str))
        (else
         (when (= 1 (modulo (string-length str) 2))
           (set! str (string-append "0" str)))
         (let lp1 ((n 1)
                   (s "")
                   (an '())
                   (nlst (string->list str)))

           (when (debug?)
             (unless (null? nlst)
               (debugln "lp1:")
               (debugln (list ':n (modulo n 2) ':an an ':s s ':nlst nlst))))

           (cond
             ((null? nlst)
              (when (debug?) (debugln "  ret\n"))
              (reverse an))

             ;; add numbers on even-steps
             ((and (not (= 0 n))
                   (= 0 (modulo n 2)))
              (let ((nstr (string-append s (string (car nlst)))))
                (when (debug?)
                  (debugln "  block1\n")
                  (debugln nstr))
                ;; trim 01 to 1
                (when (equal? #\0 (string-ref nstr 0))
                  (set! nstr
                        (list->string
                         (list-tail (string->list nstr) 1))))
                (lp1 (add1 n)
                     ""
                     (cons (string->number nstr)
                           an)
                     (cdr nlst))))

             ;; concatenate strings on odd-steps
             (else
              (when (debug?) (debugln "  block2\n"))
              (lp1 (add1 n)
                   (string-append s (string (car nlst)))
                   an
                   (cdr nlst))))))))

(define (wrap-with-chksm barcode)
  (when (debug?) (debugln barcode))
  (let* ((weights (vector->list
                   (vector-map
                    (lambda (i x)
                      (if (= i 0)
                          x
                          (* i x)))
                    (list->vector barcode))))
         (sum (foldl + 0 weights))
         (chk (modulo sum 103))
         (stop (index-where code-a
                         (lambda (x) (equal? x 'END)))))
    (when (debug?)
      (debugln "within wrap-with-chksum")
      (debugln (format "chk:~a" chk))
      (debugln (format "stop:~a" stop)))
    (append barcode
            (list chk stop))))

(define (get-barcode loc event user)
  (and (string? loc)
       (let ((lst
              (list 'START-CODE-A
                    loc
                    'CODE-C
                    ;;coerce to strings if numbers
                    ;;
                    ;;consider padding it by 0s similarly to the example
                    ;;HAWKSWELL
                    (or (and (string? event) event)
                        (number->string event))
                    'FNC1 ;; make reading between fields easier
                    (or (and (string? user) user)
                        (number->string user)))))
         (let lp ((code-sym 'code-a)
                  (code code-a)
                  (lst lst)
                  (a '()))
           (when (debug?)
             (unless (null? lst)
               (debugln "lp:")
               ;;pretty-print
               (debugln (list code-sym lst a))))
           (cond
             ((null? lst)
              (wrap-with-chksm (reverse a)))
             ;; move from code-a to code-c
             ((eq? (car lst) 'CODE-C)
              (lp 'code-c code-c (cdr lst)
                  (cons (index-where code (lambda (x) (equal? x (car lst))))
                        a)))
             ;; reduce and add codes to `a'
             (else
              (define (next-elem)
                (when (debug?) (debugln (list ':car (car lst))))
                (case code-sym
                  ('code-a
                   (map (lambda (q)
                          (when (debug?) (debugln (list ':q q)))
                          (index-where code (lambda (x) (equal? q x))))
                        (if (symbol? (car lst))
                            (list (car lst))
                            (string->list (car lst)))))
                  ('code-c
                   ;; is not the same as CODE-C
                   (map (lambda (q)
                          (when (debug?) (debugln (list ':q q)))
                          (index-where code (lambda (x) (equal? q x))))
                        (chars->num (car lst))))))
              (let ((elem (next-elem)))
                (when (debug?)
                  (debugln (list ':elem elem))
                  (map (lambda (x) (debugln (list ':code
                                                  (list-ref code x))))
                       elem))
                (lp code-sym code
                    (cdr lst)
                    ;;replace with cons to have nested list
                    (append (reverse elem)
                            a)))))))))

(define (get-widths barcode)
  (map (lambda (x) (list-ref widths x))
       barcode))

(define debug? (make-parameter #f))
(define lengths? (make-parameter #f))
(when (lengths?)
 (displayln ;;pretty-print
   (list ':widths (length widths)
         ':code-a (length code-a)
         ':code-b (length code-b)
         ':code-c (length code-c))))


;;; USAGE
;;; ./barcode.rkt HWK 00003 000000023
;;; =>
#||
{"barcode":         [ 41, 56, 44, 99, 0, 0, 3, 102, 0, 0, 0, 0, 23 ],
  "barcode-widths": [ 231311, 331121, 132131, 113141, 212222, 212222,
                      121223, 411131, 212222, 212222, 212222, 212222, 312131]}
||#
(define input (list))
(define barcode
  (cond ((= 0 (vector-length (current-command-line-arguments)))
         (when (debug?) (debugln "HWK 00001 0000000001"))
         (set! input (list "HWK" "00001" "0000000001"))
         (apply get-barcode input))

        ((let ((f (vector-ref (current-command-line-arguments) 0)))
           (or (string=? f "--help")
               (string=? f "-h")))
         (map (lambda (x) (displayln x))
              '("---------------------------  USAGE  ---------------------------"
                "./barcode.rkt location event userid"
                "  location :: String"
                "  event    :: Int"
                "  userid   :: Int"
                " ->"
                "  String   :: JSON"
                "--------------------------- EXAMPLE ---------------------------"
                "$ ./barcode.rkt HWK 00003 000000023"
                "{"
                "  \"barcode\": ["
                "    41, 56, 44, 99, 0, 0, 3, 102, 0, 0, 0, 0, 23, 60, 108 "
                "  ],"
                "  \"barcode_widths\": ["
                "    211412, 231311, 331121, 132131, 113141, 212222, 212222,"
                "    121223, 411131, 212222, 212222, 212222, 212222, 312131,"
                "    314111, 2331112"
                "  ],"
                " \"input\": ["
                "    \"HWK\", \"00003\", \"000000023\""
                "}"))
         (exit 0))

      (else
       (let ((args (vector->list (vector-take (current-command-line-arguments) 3))))
         (set! input (cons (string-upcase (car args)) ;; hwk -> HWK
                           (cdr args)))
         (apply get-barcode input)))))

(define (dump-barcode)
  (let ()
    (write-json
     (hasheq 'barcode barcode
             'input input
             'barcode_widths barcode-widths))
    (newline)))

(define barcode-widths (get-widths barcode))
;(dump-state #f)
(provide dump-state)
