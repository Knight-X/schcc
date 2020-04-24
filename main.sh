#lang racket
(define env0 '())


(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
        [(not p) #f]
        [else (cdr p)]))))

(struct Closure (f env))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))


(define interp
  (lambda (exp env)
    (match exp
      [(? number? x) x]
      [(? symbol? x) (begin (display "symbol") (newline)(let ([v (lookup x env)])
                       (cond
                         [(not v)
                          (error "undefined variable" x)]
                         [else v])))]
      [`(eq? ,e1 ,e2)
         (begin (display "eq") (display env) (newline)(let ([g (interp e1 env)] [s (interp e2 env)]) (= g s)))]
      [`(if ,e1 ,e2 ,e3)
        (begin (display "if")(let ([v1 (interp e1 env)])
          (if (not v1) (begin (display v1)(display "e3")(interp e3 env)) (begin (display "e2")(interp e2 env)) )))]
      [`(letrec ([,x ,e1]) ,e2)
          (begin (display "go") (display env) (newline)
                 (let ([v1 (match e1
                             [`(lambda (,g) ,s)
                              (Closure e1 env)])])
             (begin (display "exe rec") (display env) (newline)(interp e2 (ext-env x v1 env)))))]
      [`(let ([,x ,e1]) ,e2)
       (let ([v1 (interp e1 env)])
         (interp e2 (ext-env x v1 env)))]
      [`(lambda (,x) ,e)
       (begin (display "lambda")(display env) (newline)
         (Closure exp env))]
      [`(,e1 ,e2)
       (begin (display "eva") (display env) (display e1) (display e2)(newline) (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (begin (display "evan")(display v1)(match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (begin (display "new env")(display env-save)(interp e (ext-env x v2 (ext-env e1 v1 env-save))))]))))]
      [`(,op ,e1 ,e2)
       (begin (display "+") (newline)(let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)])))])))
