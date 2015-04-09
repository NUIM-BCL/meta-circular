#lang racket

(define (env-binding id val)
  (cons id val))

(define (env-bindings env)
  (cdr env))

(define (env-lookup env id)
  (match (assoc id (env-bindings env))
    [(cons v l) l]
    [_ #f]))

(define (env-lift-count env)
  (car env))

(define (env-make lift-count bindings)
  (cons lift-count bindings))

(define (env-extend env ids vals)
  (if (not (= (length ids) (length vals)))
      (error "env-extend: length mismatch between ids and vals" ids vals)
      (let ((bindings (map (lambda (id val) (env-binding id val)) ids vals)))
        (env-make (env-lift-count env) (append bindings (env-bindings env))))))

(define (%lift f)
  (match f
    [(list 'closure params body cenv)
     (list 'closure params body (env-lift cenv))]
    [(? procedure?) f]
    [_ (error "%lift: could not lift" f)]))

(define (%bundle t1 t2)
  `(bundle ,t1 ,t2))

(define (%tang t)
  (match t
    [(list 'bundle b1 b2) b2]
    [_ (error "%tang: expected a bundle in tang instead of" t)]))

(define (%primal t)
  (match t
    [(list 'bundle b1 b2) b1]
    [_ (error "%primal: Expected a bundle in primal instead of" t)]))

(define (%sin a)
  (match a
    [(? number?) (sin a)]
    [(list 'bundle a1 a2) `(bundle ,(%sin a1) ,(%* a2 (%cos a1)))]
    [_ (error "%sin: Expecting a number or bundle instead of" a)]))

(define (%cos a)
  (match a
    [(? number?) (cos a)]
    [(list 'bundle a1 a2) `(bundle ,(%sin a1) ,(%* a2 (%* -1 (%sin a1))))]
    [_ (error "%sin: Expecting a number or bundle instead of" a)]))

(define (%exp a)
  (match a
    [(? number?) (exp a)]
    [(list 'bundle a1 a2) (let ((z (%exp a1)))
                            `(bundle ,z ,(%* a2 z)))]
    [_ (error "lexp: Expecting a num or bundle instead of" a)]))

(define (%+ a b)
  (match* (a b)
    [((? number?) (? number?)) (+ a b)]
    [((list 'bundle a1 a2) (list 'bundle b1 b2))
     `(bundle ,(%+ a1 b1) ,(%+ a2 b2))]
    [(_ _) (error "%+: Expecting bundle or number instead of" a "and" b)]))

(define (%* a b)
  (match* (a b)
    [((? number?) (? number?)) (* a b)]
    [((list 'bundle a1 a2) (list 'bundle b1 b2))
     `(bundle ,(%* a1 b1) ,(%+ (%* a1 b2) (%* a2 b1)))]
    [((? number?) (list 'bundle b1 b2)) `(bundle ,(%* a b1) ,(%* a b2))]
    [((list 'bundle a1 a2) (? number?)) `(bundle ,(%* b a1) ,(%* b a2))]
    [(_ _) (error "%*: Expecting bundle or number instead of" a "and" b)]))

(define (lift-numeric-as-const-n n c)
  (cond [(= n 0) c]
        [(> n 0) (lift-numeric-as-const-n (- n 1) (lift-numeric-as-const c))]))

(define (lift-numeric-as-const n)
  `(bundle ,n ,(zero-out-numeric n)))

(define (zero-out-numeric n)
  (match n
    [(? number? ) 0]
    [(list 'bundle t1 t2) (list 'bundle (zero-out-numeric t1)
                                        (zero-out-numeric t2))]
    [_ (error "zero-out-numeric: Expecting num or bundle instead of" n)]))

(define (env-lift env)
  (let ([lifted-bindings
         (map (lambda (binding)
                (match binding
                  [(cons var val)
                   (cond [(procedure? val) binding]
                         [(eq? (car val) 'closure)
                          (match val
                            [(list 'closure params body cenv)
                             (env-binding var `(closure ,params
                                                        ,body
                                                        ,(env-lift cenv)))]
                            [_ (error "env-lift: expecting a closure instead of" val)])]
                         [(or (eq? (car val) 'bundle)
                              (number? val))
                          (env-binding var (lift-numeric-as-const val))]
                         [#t (error "env-lift: unknown element" binding "in env")])]
                  [_ (error "env-lift: expecting binding instead of" binding)]))
              (env-bindings env))])
    (env-make (+ 1 (env-lift-count env)) lifted-bindings)))

(define (%eval expr env)
  (match expr
    [(list 'let id '= val-expr 'in body)
     (%eval body
           (env-extend env
                       (list id)
                       (list (%eval val-expr env))))]
    [(? number?) (lift-numeric-as-const-n (env-lift-count env) expr)]
    [(? symbol?) (or (env-lookup env expr)
                     (error ("%eval: failed to look up identifier" expr)))]
    [(list 'lambda params body) `(closure (,@params) ,body ,env)]
    [(list f args ...) (%apply (%eval f env)
                               (map (lambda (arg) (%eval arg env)) args))]
    [_ (error "%eval: failed to match expression" expr)]))

(define (%apply f args)
  (match f
    [(list 'closure params body cenv)
     (%eval body (env-extend cenv
                            params
                            args))]
    [(? procedure?) (apply f args)]
    [_ (error "%apply: cannot apply" f "to" args)]))


(define proto-env
  (let* ([proto-env
          (env-extend (env-make 0 '())
                      '(sin cos exp + * bundle tang primal lift)
                      (list %sin %cos %exp %+ %* %bundle %tang %primal %lift))]
         [proto-env
          (env-extend proto-env
                      '(D D2 diff)
                      (list
                       (%eval '(lambda (f a) (tang ((lift f) (bundle a 1))))
                             proto-env)
                       (%eval '(lambda (f a)
                                (tang (tang ((lift (lift f))
                                             (bundle (bundle a 1)
                                                     (bundle 1 0))))))
                             proto-env)
                       (%eval '(lambda (f) (lambda (a) (tang ((lift f) (bundle a 1)))))
                             proto-env)))])
    proto-env))

(define (eval expr) (%eval expr proto-env))

(provide eval)
