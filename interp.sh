(define interp
  (lambda (exp env)
    (match exp
      [(? number? x) x]
      [(? symbol? x) (begin (display "symbol")(let ([v (lookup x env)])
                       (cond
                         [(not v)
                          (error "undefined variable" x)]
                         [else v])))]
      [`(eq? ,e1 ,e2)
         (begin (display "eq") (display env) (newline)(eq? e1 e2))]
      [`(if ,e1 ,e2 ,e3)
        (let ([v1 (interp e1 env)])
          (if (eq? v1 #t) (interp e2 env) (interp e3 env)))]
      [`(letrec ([,x (lambda (,s) ,e1)]) ,e2)
          (begin (display "go") (display env) (newline) (let ([v1 (interp e1 (ext-env x `procedure env))])
             (begin (display "exe rec") (display env) (newline)(interp e2 (ext-env x v1 env)))))]
      [`(let ([,x ,e1]) ,e2)
       (let ([v1 (interp e1 env)])
         (interp e2 (ext-env x v1 env)))]
      [`(lambda (,x) ,e)
       (begin (display "lambda")(display env) (newline)(if (eq? (check (flatten e) env) 'procedure)
         (Closure exp (ext-env  (have-defined (flatten e) env) (Closure exp env) env))
       (display "come")))]
      [`(,e1 ,e2)
       (begin (display "eva") (display env) (newline) (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (interp e (ext-env x v2 env-save))])))]
      [`(,op ,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))
