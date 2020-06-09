#lang racket
(require "parse.rkt")

(define cps-of-exps
  (lambda (exps builder)
    (let cps-of-rest ((exps exps))
      (begin (newline) (display "cps-exps")
             (display exps)
      (let ([pos (list-index
             (lambda (exp)
                     (not (is-exp-simple? exp)))
                exps)])
        (begin (display pos)
      (if (not pos)
         (builder (map cps-of-simple-exp exps))
         (let ([var (fresh-identifier 'var)])
         (cps-of-exp
          (list-ref exps pos)
                    `(lambda ,(list var) ,(cps-of-rest (list-set exps pos (append (list 'identifier:) (list var))))))))))))))

(define cps-of-exp
  (lambda (exp cont)
    (begin
      (newline)
      (display "cps-of-exp")
      (display exp)
      (newline)
    (match exp
      [`(const: ,x) (make-send-to-cont cont x)]
      [`(identifier: ,x) (make-send-to-cont cont x)]
      [`(lambda-exp: ,x ,e) (let ([new_var (append (list x) 'k%00)]
                               [new_body (append (list e) 'k%00)])
                               (make-send-to-cont cont
                                            `(lambda (,new_var) (,new_body))))]
      
      [`(lambda-call-exp: ,e1 ,e2) (cps-of-call-exp e1 e2 cont)]))))

(define make-send-to-cont
  (lambda (cont bexp)
    (begin
      (newline)
      (display "make-send")
      (display cont)
      (display bexp)
      (newline)
   `(,cont ,(list bexp)))))

(define cps-of-call-exp
  (lambda (rator rands cont)
    (begin (newline)
           (display "cps-call")
           (display "rands")
           (display rands)
           (newline)
           (display cont)
    (cps-of-exps (cons rator rands)
                 (lambda (new-args)
                   (begin (newline) (display "call builder") (display (car new-args) ) `(,(car new-args) ,(append (cdr new-args)  (list cont)))))))))
(define is-exp-simple?
  (lambda (exp)
    (begin
      (newline)
      (display "is-simple")
      (display exp)
    (match exp
      [`(const: ,x) (begin (display "const ")#t)]
      [`(identifier: ,x) (begin (display "identifier ")#t)]
      [`(lambda-call-exp: ,e1 ,e2) (begin (display "call"))#f]
      [`(lambda-exp: ,x ,e) (begin (display "lambda ")#t)]
      [_ (begin (display "else")#f)]))))

(define cps-of-simple-exp
  (lambda (exp)
    (begin
      (newline)
      (display "simple-exp")
      (display exp)
    (match  exp
     [`(const: ,x) exp]
     [`(identifier: ,x) exp]
     [`(lambda-exp: ,x ,e) (let ([new_x (append  x '(k%00))]
                              [new_body (cps-of-exp e 'k%00)])
                          `(lambda ,new_x ,new_body))]))))


(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"             ; this can't appear in an input identifier
        (number->string sn))))))

(define list-set
  (lambda (lst n val)
    (cond
      ((null? lst) (display 'list-set "ran off end"))
      ((zero? n) (cons val (cdr lst)))
      (else (cons (car lst) (list-set (cdr lst) (- n 1) val))))))
(define list-index2
  (lambda (pred lst)
    (newline)
    (begin (display "index2")
    (cond
      ((null? lst) #f)
      ((pred lst) 0)
      (else #f)))))
(define list-index
  (lambda (pred lst)
    (begin
      (newline)
      (display "list-index")
      (display lst)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) 0)
      ((list-index pred (cdr lst)) => (lambda (n) (+ n 1)))
      (else #f)))))