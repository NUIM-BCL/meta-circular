#lang racket

(define (eval expr env)
  (match expr
    [(list 'bundle t1 t2) `(bundle ,(eval t1 env) ,(eval t2 env))]
    [(list 'tang t) (let ([tp (eval t env)])
                      (match tp
                        [(list 'bundle b1 b2) b2]
                        [_ (error "eval: Expected a bundle in dual instead of" tp)]))]
    [(list 'primal t) (let ([tp (eval t env)])
                        (match tp
                          [(list 'bundle b1 b2) b1]
                          [_ (error "eval: Expected a bundle in primal instead of" tp)]))]
    [(? symbol?) (or (lookup env expr)
                     (error ("eval: Failed to look up identifier" expr)))]
    [(? number?) expr]
    [(list 'lambda params body) `(closure (,@params) ,body ,env)]
    [(list f args ...) 
     (let ([fp (eval f env)]
           [argsp (map (lambda (a) (eval a env)) args)])
       (match fp
         [(list 'closure params body cenv)
          (eval body (extend-env cenv
                                 params
                                 argsp))]
         [(? procedure?) (apply fp argsp)]
         [_ (error "eval: Cannot apply" fp)]))]
    [_ (error "eval: Failed to match expression" expr)]))

(define (lookup env id)
  (match (assoc id env)
    [(cons v l) l]
    [_ #f]))

(define (extend-env env ids vals)
  (if (not (= (length ids) (length vals)))
      (error "extend-env: length mismatch between ids and vals" ids vals)
      (let ((bindings (map (lambda (id val) (binding id val)) ids vals)))
        (append bindings env))))

(define (binding id val)
  (cons id val))

