#lang racket

;;; Camogie supports the following:

;; - elementary arithemtic
;; - let expressions
;; - if expressions
;; - closures
;; - booleans

(define proto-env (list (cons '+ +) 
                        (cons '- -)
                        (cons '/ /)
                        (cons '* *)
                        (cons '= =)
                        (cons '< <)
                        (cons '> >)
                        (cons '<= <=)
                        (cons '>= >=)))

(define (eval expr env)
  (match expr
    [(? number?) expr]
    [(? boolean?) expr]
    [(? symbol?) (lookup env expr)]
    [(list 'if condition true-branch false-branch) (if (eval condition env)
                                                       (eval true-branch env)
                                                       (eval false-branch env))]
    [(list 'lambda params body) (list 'closure params body env)]
    [(list 'let id '= val-expr 'in body) (eval body 
                                               (extend-env env
                                                           (list id)
                                                           (list (eval val-expr env))))]
    [(list f args ...) (let ((fp (eval f env))
                             (fargs (map (lambda (a) (eval a env)) args)))
                         (match fp
                           [(list 'closure fparams fbody fenv)
                            (eval fbody (extend-env fenv
                                                    fparams
                                                    fargs))]
                           [_ (apply fp fargs)]))]
    [_ (error "eval: Failed to match expression: " expr)]))

(define (lookup env id)
  (match (assoc id proto-env)
    [(cons v l) l]
    [_ (match (assoc id env)
         [(cons v l) l]
         [_ (error "lookup: Failed to lookup identifier: " id)])]))

(define (extend-env env ids vals)
  (let ((bindings (map (lambda (id val) (binding id val)) ids vals)))
    (append bindings env)))

(define (binding id val)
  (cons id val))

