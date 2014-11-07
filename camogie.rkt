#lang racket

(define (new+ a b)
  (if (and (number? a)
           (number? b))
      (+ a b)
      (match a
        [(list (list 'bundle a1) a2)
         (match b
           [(list (list 'bundle b1) b2)
            `((bundle ,(new+ a1 b1)) ,(new+ a2 b2))]
           [_ (error "new+: Expecting a bundle for second argument instead of" 
                     b)])]
        [_ (error "new+: Expecting a bundle for first argument instead of" 
                  a)])))

(define (new* a b)
  (if (and (number? a)
           (number? b))
      (* a b)
      (match a
        [(list (list 'bundle a1) a2)
         (match b
           [(list (list 'bundle b1) b2)
            `((bundle ,(new* a1 b1)) ,(new* a2 b2))]
           [_ (error "new*: Expecting a bundle for second argument instead of" 
                     b)])]
        [_ (error "new*: Expecting a bundle for first argument instead of"
                  a)])))

(define (eval expr env)
  (match expr
    ['sin sin]
    ['cos cos]
    [(list (list '+ t1) t2) (new+ (eval t1 env) (eval t2 env))]
    [(list (list '* t1) t2) (new* (eval t1 env) (eval t2 env))]
    [(list (list 'bundle t1) t2) `((bundle ,(eval t1 env)) 
                                   ,(eval t2 env))]
    [(list 'dual t) (let ([tp (eval t env)])
                      (match tp
                        [(list (list 'bundle b1) b2) b2]
                        [_ (error "eval: Expected a bundle instead of" tp)]))]
    [(list 'primal t) (let ([tp (eval t env)])
                        (match tp
                          [(list (list 'bundle b1) b2) b1]
                          [_ (error "eval: Expected a bundle instead of" tp)]))]
    [(? symbol?) (or (lookup env expr)
                     (error ("eval: Failed to look up identifier" expr)))]
    [(? number?) expr]
    [(list 'lambda param body) `(closure ,@param ,body ,env)]
    [(list f arg) 
     (let ([fp (eval f env)]
           [argp (eval arg env)]) 
       (match fp
         [(list 'closure param body cenv)
          (eval body (extend-env cenv
                                 param
                                 argp))]
         [(? procedure?) (apply fp (list argp))]
         [_ (error "eval: Expected a closure instead of" fp)]))]
    [_ (error "eval: Failed to match expression" expr)]))

(define (lift expr)
  (match expr
    [(? number?) `((bundle ,expr) 0)]
    ['dual `(lambda (x) (dual x))]
    ['primal `(lambda (x) (primal x))]
    ['bundle `(lambda (x) (lambda (y) ((bundle x) y)))]
    ['sin `(lambda (x) ((bundle (sin (primal x)))
                   ((* (dual x)) 
                    (cos (primal x)))))]
    ['cos `(lambda (x) ((bundle (cos (primal x)))
                   ((* (dual x)) 
                    ((* -1)(sin (primal x))))))]
    ['+ `(lambda (x) (lambda (y) ((bundle ((+ (primal x)) (primal y))) 
                        ((+ (dual x)) (dual y)))))]
    ['* `(lambda (x) (lambda (y) ((bundle ((* (primal x)) (primal y)))
                                ((+ ((* (primal x)) (dual y)))
                                 ((* (primal y)) (dual x))))))]
    [(list 'lambda _ _)
     (let ([disassembled-lambdas (disassemble-lambdas expr)])
       (match disassembled-lambdas
         [(list vars (list body))
          (cond [(member body vars)(assemble-lambdas vars body)]
                [(not (free-vars? vars body))
                 (assemble-lambdas vars (lift body))]
                [#t (assemble-lambdas 
                     vars
                     `(,(assemble-application 
                         (lift (assemble-lambdas vars (first body)))
                         vars)
                       ,(assemble-application 
                         (lift (assemble-lambdas vars (second body)))
                         vars)))])]
         [_ (error "lift: Expecting a lambda instead of" expr)]))]
    [(list f arg) `(,(lift f),(lift arg))]))

;;; Input: (lambda (x) (lambda (y) ... (lambda (z) M)))
;;; Output: ((x y ... z) (M))
(define (disassemble-lambdas expr)
  (letrec ([split-vars-from-body
            (lambda (expr) (match expr
                     [(list 'lambda (list param) body)
                      (cons param (split-vars-from-body body))]
                     [_ `((,expr))]))])
    (let-values ([(vars rest) 
                  (splitf-at (split-vars-from-body expr) symbol?)])
      (cons vars rest))))

;;; Input: (x y ... z) M
;;; Output: (lambda (x) (lambda (y) ... (lambda (z) M)))
(define (assemble-lambdas vars body)
  (letrec ([merge-vars-and-body
            (lambda (vars body)
              (if (null? vars)
                  body
                  `(lambda (,(car vars)) ,(merge-vars-and-body (rest vars) 
                                                          body))))])
    (merge-vars-and-body vars body)))

;;; Input: M (x y ... z)
;;; Output: (((M x) y) ... z)
(define (assemble-application body vars)
  (foldl (lambda (v acc) (list acc v)) body vars))

(define (free-vars? vars expr)
  (ormap (lambda (p) (free-var? p expr)) vars))

(define (free-var? var expr)
  (define (free-var-helper var expr stack)
    (match expr
      [(? number?) #f]
      [(? symbol?) (if (eq? var expr)
                       (if (member var stack) #f #t)
                       #f)]
      [(list 'lambda (list param) body) 
       (free-var-helper var body (cons param stack))]
      [(list t1 t2) (or (free-var-helper var t1 stack) 
                        (free-var-helper var t2 stack))]
      [_ (error "free?: Unrecognized expression" expr)]))
  (free-var-helper var expr '()))

(define (lookup env id)
  (match (assoc id env)
    [(cons v l) l]
    [_ #f]))

(define (extend-env env id val)
  (cons (binding id val) env))

(define (binding id val)
  (cons id val))
