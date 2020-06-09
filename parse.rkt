#lang racket

(struct identifier (identi))

(struct add-exp (var1 var2))
(struct const-exp (x))
(struct lambda-exp (var1 exp1))
(struct lambda-call-exp (exp1 exp2))
(provide a-program identifier)

(define scan
  (lambda (exp)
    (begin (display exp)
           (newline))
    (if (null? exp)
        '()
    (match exp
      ['lambda (list 'lambda)]
      ['+ (begin (display "+")) (list '+)]
      ['- (begin (display "-")) (list '-)]
      [(? symbol? x) (begin (display "symbol")) (list (identifier x))]
      [(? number? x) (begin (display "number")) (list x)]
      [_  (begin (display "_") (newline))   (append  (if (list? (car exp)) (list (scan (car exp))) (scan (car exp)))
                                                     (if (not (null?  (cdr exp))) (scan (cdr exp)) '()))]
       ))))

(define parse
    (lambda (exp)
      (begin (display exp)
      (match exp
        [(? number? x) (const-exp x)]
        [(identifier a) exp]
        [`(lambda  (,x) ,e) (lambda-exp (parse x) (parse e))]
        [`(,e1 ,e2) (lambda-call-exp (parse e1) (parse e2))]
        [`(,op ,e1 ,e2)
           (let ([var1 (parse e1)]
                 [var2 (parse e2)])
                 (match op
                   ['+ (add-exp var1 var2)]))]
         [_ (parse (car exp))]))))

(define printc
    (lambda (exp)
      (newline)
      (display exp)
      (newline)
      (match exp
        [(identifier a) (append (list 'identifier:) (list a))]
        [(const-exp b) (append (list 'const:) (list b))]
        [(add-exp var1 var2) (append (list 'add-exp:) `(,(printc var1)) `(,(printc var2)))]
        [(lambda-exp g y) 
                          (append (list 'lambda-exp:)
                                   `(,(list (identifier-identi g)))
                                  `(,(printc y)))]
        [(lambda-call-exp a b)
                          (append (list 'lambda-call-exp:)
                                  `(,(printc a))
                                  `((,(printc b))))])))

(define a-program
  (lambda (exp)
  `(,(printc (parse (scan exp))))))

