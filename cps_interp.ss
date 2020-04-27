
(define apply-cont
  (lambda (cont v)
    (cont v)))

(define let-exp-cont
  (lambda (var body env cont)
    (lambda (val)
       (interp body (ext-env var val env)))))

(define if-test-cont
  (lambda (exp2 exp3 env cont)
    (lambda (val)
      (if (not v1) (begin (display v1)(display "e3")(interp e3 env cont)) 
        (begin (display "e2")(interp e2 env cont))))))

(define interp
  (lambda (exp env cont)
    (match exp
      [(? number? x) (apply-cont cont x)]
      [(? symbol? x) (apply-cont cont (begin (display "symbol") (newline)(let ([v (lookup x env)])
                       (cond
                         [(not v)
                          (error "undefined variable" x)]
                         [else v]))))]
      [`(eq? ,e1 ,e2)
         (begin (display "eq") (display env) (newline)(let ([g (interp e1 env)] [s (interp e2 env)]) (= g s)))]
      [`(if ,e1 ,e2 ,e3)
        (begin (display "if")
         (interp e1 env (if-test-cont e2 e3 env cont)))]
      [`(letrec ([,x ,e1]) ,e2)
          (begin (display "go") (display env) (newline)
                 (let ([v1 (match e1
                             [`(lambda (,g) ,s)
                              (Closure e1 env)])])
             (begin (display "exe rec") (display env) (newline)(interp e2 (ext-env x v1 env) cont))))]
      [`(let ([,x ,e1]) ,e2)
         (interp e1 env (let-exp-cont x e2 env cont))]
      [`(lambda (,x) ,e)
       (begin (display "lambda")(display env) (newline)
         (apply-cont cont (Closure exp env))]
      [`(,e1 ,e2)
       (begin (display "eva") (display env) (display e1) (display e2)(newline) (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (begin (display "evan")(display v1)(match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (begin (display "new env")(display env-save)(interp e (ext-env x v2 (ext-env e1 v1 env-save))))]))))]
      [`(,op ,e1 ,e2)
       (begin (display "+") (newline)
             
         (match op
           ['+ (interp e1 env (plus-cont1 e2 env cont))]
           ['- (interp e1 env (minus-cont1 e2 env cont))]
           ['* (interp e1 env (multi-cont1 e2 env cont))]
           ['/ (interp e1 env (divide-cont1 e2 env cont))]))])))

(define end-cont
  (lambda ()
    (lambda (val)
      (begin
        (display "end of continuation")
        val))))
