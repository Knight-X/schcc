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

(define check
    (lambda (x env)
      (if (eq? x '()) #f
          (let ([v1 (lookup (car x) env)])
            (if (not v1)
                (check (cdr x) env)
                v1)))))

(define check
  (lambda (x env)
   (if (eq? x '()) #f
    (let ([p (car x)]
          [g (cdr x)])
             (let ([v (assq p env)]
                  [s (assq g env)])
              (cond
                 [(or (not (null? s)) (not (null? v))) #t]
                 [else (begin (unless (list? (car p)) (check (car p) env))
                              (unless (list? (cdr p)) (check (cdr p) env))
                              (unless (list? (car g)) (check (car g) env))
                              (unless (list? (cdr g)) (check (cdr g) env)))]))))))
(define have-defined
  (lambda (x env)
    (if (eq? x '()) '() 
         (let ([p (car x)])
          (let ([v (assq p env)])
         (cond [(not v) (have-defined (cdr x) env)] 
          [else p]) 
          )))))
(define interp
  (lambda (exp env)
    (match exp
      [(? number? x) x]
      [(? symbol? x) (let ([v (lookup x env)])
                       (cond
                         [(not v)
                          (error "undefined variable" x)]
                         [else v]))]
      [`(eq? ,e1 ,e2)
         (eq? e1 e2)]
      [`(if ,e1 ,e2 ,e3)
        (let ([v1 (interp e1 env)])
          (if (eq? v1 #t) (interp e2 env) (interp e3 env)))]
      [`(letrc ([,x ,e1]) ,e2)
          let ([v1 (interp e1 (ext-env x `procedure env))])
             (interp e2 (ext-env x v1 env))] 
      [`(let ([,x ,e1]) ,e2)
       (let ([v1 (interp e1 env)])
         (interp e2 (ext-env x v1 env)))]
      [`(lambda (,x) ,e)
       (if (eq? (check e env) #t)
        (Closure exp (ext-env (have-defined e env) (Clousr exp env) env))
       (Closure exp env))]
      [`(,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (interp e (ext-env x v2 env-save))]))]
      [`(,op ,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))
