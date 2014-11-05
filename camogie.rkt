#lang racket

(define (new+ a b)
  (if (and (number? a)
           (number? b))
      (+ a b)
      (match a
        [(list (list 'bundle a1) a2)
         (match b
           [(list (list 'bundle b1) b2)
            `((bundle ,(new+ a1 b1)) ,(new+ a2 b2))])])))

(define (new* a b)
  (if (and (number? a)
           (number? b))
      (* a b)
      (match a
        [(list (list 'bundle a1) a2)
         (match b
           [(list (list 'bundle b1) b2)
            `((bundle ,(new* a1 b1)) ,(new* a2 b2))])])))

(define (eval expr env)
  (match expr
    ['sin sin]
    ['cos cos]
    [(list (list '+ t1) t2) (new+ (eval t1 env) (eval t2 env))]
    [(list (list '* t1) t2) (new* (eval t1 env) (eval t2 env))]
    [(list (list 'bundle t1) t2) `((bundle ,(eval t1 env)) ,(eval t2 env))]
    [(list 'dual t) (let ((tp (eval t env)))
                      (match tp
                        [(list (list 'bundle b1) b2) b2]
                        [_ "eval: Expected a bundle instead of" tp]))]
    [(list 'primal t) (let ((tp (eval t env)))
                        (match tp
                          [(list (list 'bundle b1) b2) b1]
                          [_ "eval: Expected a bundle instead of" tp]))]
    [(? symbol?) (or (lookup env expr)
                     (error ("eval: Failed to look up identifier" expr)))]
    [(? number?) expr]
    [(list 'lambda param body) `(closure ,@param ,body ,env)]
    [(list f arg) 
     (let ((fp (eval f env))
           (argp (eval arg env))) 
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
    ['+ `(lambda (x) (lambda (y) ((bundle ((+ (primal x)) (primal y))) ((+ (dual x)) (dual y)))))]
    ['* `(lambda (x) (lambda (y) ((bundle ((* (primal x)) (primal y)))
                                ((+ ((* (primal x)) (dual y)))
                                 ((* (primal y)) (dual x))))))]
    [(? symbol?) `((bundle ,expr) 0)]
    [(list 'lambda (list param) body)
     (cond [(and (symbol? body) (eq? body param)) `(lambda (,param) ,body)]
           [(not (free? param body)) `(lambda (,param) ,(lift body))]
           [(match body
              [(list t1 t2)
               `(lambda (,param) ((,(lift `(lambda (,param) ,t1)) ,param)
                             (,(lift `(lambda (,param) ,t2)) ,param)))]
              [#t (error "lift: Failed to lift lambda" expr)])])]
    [(list t1 t2) `(,(lift t1) ,(lift t2))]))


(define (free? var expr)
  (define (free-var-helper var expr stack)
    (match expr
      [(? number?) #f]
      [(? symbol?) (if (eq? var expr)
                       (if (member var stack) #f #t)
                       #f)]
      [(list 'lambda (list param) body) (free-var-helper var body (cons var stack))]
      [(list t1 t2) (or (free-var-helper var t1 stack) (free-var-helper var t2 stack))]
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
