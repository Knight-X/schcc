
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

(define diff1-cont 
  (lambda (exp2 env cont) 
    (lambda (val1)
      (begin (display "diff1-cont") (interp exp2 env (diff2-cont val1 cont))))))

(define diff2-cont
  (lambda (val1 cont) 
    (lambda (val2)
      (begin (display "diff2-cont") (apply-cont cont (- val1 val2))))))

(define plus1-cont
  (lambda (exp2 env cont)
    (lambda (val1)
      (begin (display "plus1-cont") (interp exp2 env (plus2-cont val1 cont))))))

(define plus2-cont
  (lambda (val1 cont)
    (lambda (val2)
      (begin (display "plus2-cont") (apply-cont cont (+ val1 val2))))))

(define rator-cont  
  (lambda (rand env cont)
    (lambda (val1)
      (interp rand env (rand-cont val1 cont)))))

(define rand-cont 
  (lambda (val1 cont)
    (lambda (val2)
      (apply-procedure val1 val2 cont))))

(define apply-procedure
  (lambda (val1 val2 cont)
    (match val1
     [`(lambda (,x) ,e1) 
        (interp e1 (extend-env x val2 saved-env))])))

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
         (apply-cont cont (Closure exp env)))]
      [`(,e1 ,e2)
       (begin (display "eva") (display env) (display e1) (display e2)(newline) 
         (begin (display "evan")(display v1)
            (interp e1 env (rator-cont e2 env cont))))]
      [`(,op ,e1 ,e2)
       (begin (display "+") (newline)
             
         (match op
           ['+ (interp e1 env (plus1-cont e2 env cont))]
           ['- (interp e1 env (diff1-cont e2 env cont))]
           ['* (interp e1 env (multi-cont1 e2 env cont))]
           ['/ (interp e1 env (divide-cont1 e2 env cont))]))])))

(define end-cont
  (lambda ()
    (lambda (val)
      (begin
        (display "end of continuation")
        val))))
(define r2
  (lambda (exp)
    (interp exp env0 (end-cont))))