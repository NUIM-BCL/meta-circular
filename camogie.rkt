#lang racket

;;;;;;;;;;;;; First Attempt (Not so good) ;;;;;;;;;;;;;;;;

(define (new+ a b)
  (if (and (number? a)
           (number? b))
      (+ a b)
      (match a
        [(list 'bundle a1 a2)
         (match b
           [(list 'bundle b1 b2)
            `(bundle (new+ ,a1 ,b1) (new+ ,a2 ,b2))])])))

(define (new* a b)
  (if (and (number? a)
           (number? b))
      (* a b)
      (match a
        [(list 'bundle a1 a2)
         (match b
           [(list 'bundle b1 b2)
            `(bundle (new* ,a1 ,b1) (new* ,a2 ,b2))])])))

(define (eval expr env)
  (match expr
    ['+ new+]
    ['* new*]
    ['sin sin]
    ['cos cos]
    [(list 'diff f point)
     (let* ((liftedf (lift f))
            (pointp `(bundle ,(eval point env) 1)))
       (match liftedf
         [(list 'lambda params body)
          (eval `(,liftedf ,pointp) (lift-env env))]
         [_ (error "eval: Failed to match lifted lambda" liftedf)]))]
    [(list 'bundle a1 a2) `(bundle ,(eval a1 env) ,(eval a2 env))]
    [(list 'dual d) (let ((de (eval d env)))
                      (match de
                        [(list 'bundle a1 a2) a2]
                        [_ "eval: Failed to match evaluated dual" de]))]
    [(list 'primal p) (let ((ep (eval p env)))
                        (match ep
                          [(list 'bundle a1 a2) a1]
                          [_ "eval: Failed to match evaluated primal" ep]))]
    [(? symbol?) (or (lookup env expr)
                     (error ("eval: Failed to look up" expr)))]
    [(? number?) expr]
    [(list 'lambda params body) `(closure ,params ,body ,env)]
    [(list 'let id '= val-expr 'in body) 
     (eval body (extend-env env
                            (list id)
                            (list (eval val-expr env))))]
    [(list f args ...) 
     (let ((fp (eval f env))
           (fargs (map (lambda (a) (eval a env)) args)))
       (match fp
         [(list 'closure fparams fbody fenv)
          (eval fbody (extend-env fenv
                                  fparams
                                  fargs))]
         [_ (apply fp fargs)]))]
    [_ (error "eval: Failed to match expression:" expr)]))

(define (lift-env env)
  (let ((lifted-env (map (lambda (binding) (cons (car binding) (lift (cdr binding))))
                         env)))
    (append lifted-env env)))
    
(define (lift expr)
  (match expr
    [(? number?) `(bundle ,expr 0)]
    [(list 'bundle a1 a2) `(bundle ,(lift a1) ,(lift a2))]
    [(list 'primal b) (let ((liftedp (lift b)))
                        (match liftedp
                          [(list 'bundle a1 a2) a1]
                          [_ (error "lift: Failed to match lifted primal:" 
                                    liftedp)]))]
    [(list 'dual b) (let ((liftedp (lift b)))
                      (match liftedp
                        [(list 'bundle a1 a2) a2]
                        [_ (error "lift: Failed to match lifted dual:" 
                                  liftedp)]))]
    ['sin `(lambda (x) (bundle (sin (primal x)) (* (dual x) (cos (primal x)))))]
    ['+ `(lambda (x y) (bundle (+ (primal x) (primal y)) (+ (dual x) (dual y))))]
    ['* `(lambda (x y) (bundle (* (primal x) (primal y)) 
                          (+ (* (primal x) (dual y))
                             (* (primal y) (dual x)))))]
    [(list 'lambda params body)
     (cond [(and (symbol? body) (member body params)) `(lambda ,params ,body)]
           [(not (free-vars params body)) `(lambda ,params ,(lift body))]
           [(match body
              [(list m1 m2 n ...)
               (if (empty? n)
                   `(lambda ,params ((,(lift `(lambda ,params ,m1)) ,@params)
                                (,(lift `(lambda ,params ,m2)) ,@params)))
                   `(lambda ,params ((,(lift `(lambda ,params (,m1 ,m2))) ,@params)
                                (,(lift `(lambda ,params ,@n)) ,@params))))])]
           [#t (error "lift: Failed to lift lambda" expr)])]))

(define (free-vars vars expr)
  (andmap (lambda (p) (free-var p expr '())) vars))
            
(define (free-var var expr env)
  (match expr
    [(list 'sin arg) (free-var var arg env)]
    [(list 'cos arg) (free-var var arg env)]
    [(list '+ args ...) (ormap (lambda (p) (free-var var p env)) args)]
    [(list '* args ...) (ormap (lambda (p) (free-var var p env)) args)]
    [(list 'diff f point) (or (free-var var f env)
                              (free-var var point env))]
    [(? number?) #f]
    [(? symbol?) (if (eq? var expr)
                     (if (lookup env var) #f #t)
                     #f)]
    [(list 'bundle a1 a2) (or (free-var var a1 env) (free-var var a2 env))]
    [(list 'dual b) (free-var var b env)]
    [(list 'primal b) (free-var var b env)]
    [(list 'lambda params body) (free-var var body (extend-env env
                                                               params
                                                               (map (lambda (p) '()) params)))]))

(define (lookup env id)
  (match (assoc id env)
    [(cons v l) l]
    [_ #f]))

(define (extend-env env ids vals)
  (let ((bindings (map (lambda (id val) (binding id val)) ids vals)))
    (append bindings env)))

(define (binding id val)
  (cons id val))

