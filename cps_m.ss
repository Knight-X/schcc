#lang racket
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
                    `(lambda ,(list var) ,(cps-of-rest (list-set exps pos var))))))))))))

(define cps-of-exp
  (lambda (exp cont)
    (begin
      (newline)
      (display "cps-of-exp")
      (display exp)
      (newline)
    (match exp
      [(? number? x) (make-send-to-cont cont x)]
      [(? symbol? x) (make-send-to-cont cont x)]
      [`(lambda (,x) ,e) (let ([new_var (append (list x) 'k%00)]
                               [new_body (append (list e) 'k%00)])
                               (make-send-to-cont cont
                                            `(lambda (,new_var) (,new_body))))]
      [`(,e1 ,e2) (cps-of-call-exp e1 e2 cont)]))))

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
           (display cont)
    (cps-of-exps (cons rator rands)
                 (lambda (new-args)
                   (begin (display "call builder") (display (car new-args) ) `(,(car new-args) ,(append (cdr new-args)  (list cont)))))))))
(define is-exp-simple?
  (lambda (exp)
    (begin
      (newline)
      (display "is-simple")
      (display exp)
    (match exp
      [(? number? x) (begin (display "number")#t)]
      [(? symbol? x) (begin (display "symbol")#t)]
      [`(,e1 ,e2) (begin (display "call"))#f]
      [`(lambda (,x) ,e) (begin (display "lambda")#t)]
      [_ (begin (display "else")#f)]))))

(define cps-of-simple-exp
  (lambda (exp)
    (begin
      (newline)
      (display "simple-exp")
      (display exp)
    (match  exp
     [(? number? x) x]
     [(? symbol? x) x]
     [`(lambda (,x) ,e) (let ([new_x (append (list x) '(k%00))]
                              [new_body (cps-of-exp e 'k%00)])
                          `(lambda (,new_x) (,new_body)))]))))


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