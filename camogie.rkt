#lang racket

;;;;;;;;;;;;;;;;;;;;;; THIS DOESN'T WORK YET ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (new+ a b)
  (if (and (number? a)
           (number? b))
      (+ a b)
      (match a
        [(list 'bundle level1 a1 a2)
         (match b
           [(list 'bundle level2 b1 b2)
            (if (= level1 level2)
                `(bundle ,level1 ,(new+ a1 b1) ,(new+ a2 b2))
                (if (< level1 level2)
                    (new+ ((compose-n lift (- level2 level1)) a) b)
                    (new+ ((compose-n lift (- level1 level2)) b) a)))])])))

(define (new* a b)
  (if (and (number? a)
           (number? b))
      (* a b)
      (match a
        [(list 'bundle level1 a1 a2)
         (match b
           [(list 'bundle level2 b1 b2)
            (if (= level1 level2)
                `(bundle ,level1 ,(new* a1 b1) ,(new* a2 b2))
                (if (< level1 level2)
                    (new* ((compose-n lift (- level2 level1)) a) b)
                    (new* ((compose-n lift (- level1 level2)) b) a)))])])))


(define (compose-n func n)
  (foldr (lambda (f c) (compose f c)) func (for/list ([i (- n 1)]) lift)))
;;;;;;;;;;;;;;;;;;;;;; THIS DOESN'T WORK YET ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval expr env)
  (match expr
    ['+ new+]
    ['* new*]
    ['sin sin]
    ['cos cos]
    [(list 'diff f p) (let ((fp (eval f env))
                            (pp (eval p env)))
                        (eval `((ft ,fp) (bundle 1 ,pp 1)) env))]
    [(list 'ft c) (match c
                    [(list 'closure params body env)
                     `(closure ,params ,body ,(lift-env env))]
                    [_ (error "eval: Not lifting a closure")])]
  [(list 'bundle level a1 a2) `(bundle ,level ,(eval a1 env) ,(eval a2 env))]
    [(list 'dual d) (let ((de (eval d env)))
                      (match de
                        [(list 'bundle _ a1 a2) a2]
                        [_ "eval: Failed to match evaluated dual" de]))]
    [(list 'primal p) (let ((ep (eval p env)))
                        (match ep
                          [(list 'bundle _ a1 a2) a1]
                          [_ "eval: Failed to match evaluated primal" ep]))]
    [(? symbol?) (lookup env expr)]
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
    [(? number?) `(bundle 1 ,expr 0)]
    [(list 'bundle level a1 a2) `(bundle ,(+ level 1) ,(lift a1) ,(lift a2))]
    [(list 'primal p) (let ((liftedp (lift p)))
                        (match liftedp
                          [(list 'bundle a1 a2) a1]
                          [_ (error "lift: Failed to match lifted primal:" 
                                    liftedp)]))]
    [(list 'dual p) (let ((liftedp (lift p)))
                      (match liftedp
                        [(list 'bundle a1 a2) a2]
                        [_ (error "lift: Failed to match lifted dual:" 
                                  liftedp)]))]
    ['sin `(lambda (x) (bundle (sin (primal x)) (* (dual x) (cos (primal x)))))]
    [(list 'sin num) `(,(lift 'sin) ,(lift num))]))

    
(define (lookup env id)
  (match (assoc id env)
    [(cons v l) l]
    [_ (error "lookup: Failed to lookup identifier:" id)]))

(define (extend-env env ids vals)
  (let ((bindings (map (lambda (id val) (binding id val)) ids vals)))
    (append bindings env)))

(define (binding id val)
  (cons id val))

