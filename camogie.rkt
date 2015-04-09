#lang racket

(define proto-env '((num . 0)))

(define (eval expr env)
  (match expr
    ;; BEGIN global environment
    ['sin lsin]
    ['cos lcos]
    ['exp lexp]
    ['+ l+]
    ['* l*]
    ['lift llift]
    ['bundle bundle]
    ['tang tang]
    ['primal primal]
    ;; User-level globally-defined functions, not special forms:
    ['D2 (eval '(lambda (f a)
                  (tang (tang ((lift (lift f))
                               (bundle (bundle a (num 1))
                                       (bundle (num 1) (num 0)))))))
               proto-env)]
    ['D (eval '(lambda (f a) (tang ((lift f) (bundle a (num 1))))) proto-env)]
    ['diff (eval '(lambda (f) (lambda (x) (D f x))) proto-env)]
    ;; END global environment
    [(list 'let id '= val-expr 'in body) (eval body
                                               (extend-env env
                                                           (list id)
                                                           (list (eval val-expr env))))]
    [(list 'num n) expr (let ([c (lookup env 'num)])
                           (lift-num n c))]
    [(? symbol?) (or (lookup env expr)
                     (error ("eval: Failed to look up identifier" expr)))]
    [(list 'lambda params body) `(closure (,@params) ,body ,env)]
    [(list f args ...)
     (let ([fp (eval f env)]
           [argsp (map (lambda (arg) (eval arg env)) args)])
       (c-apply fp argsp))]
    [_ (error "eval: Failed to match expression" expr)]))

(define (c-apply fp argsp)
  (match fp
    [(list 'closure params body cenv)
     (eval body (extend-env cenv
                            params
                            argsp))]
    [(? procedure?) (apply fp argsp)]
    [_ (error "eval: Cannot apply" fp)]))

(define (llift fp)
  (match fp
    [(list 'closure params body cenv)
     (list 'closure params body (lift-env cenv))]
    [(? procedure?) fp]
    [_ (error "Could not lift" fp)]))

(define (bundle p t)
  `(bundle ,p ,t))

(define (primal tp)
  (match tp
    [(list 'bundle b1 b2) b1]
    [_ (error "primal: Expected a bundle instead of" tp)]))

(define (tang tp)
  (match tp
    [(list 'bundle b1 b2) b2]
    [_ (error "tang: Expecting a bundle instead of" tp)]))

(define (lsin a)
  (match a
    [(list 'num a1) `(num ,(sin a1))]
    [(list 'bundle a1 a2) (bundle (lsin a1) (l* a2 (lcos a1)))]
    [_ (error "lsin: Expecting a num or bundle instead of" a)]))

(define (lcos a)
  (match a
    [(list 'num a1) `(num ,(cos a1))]
    [(list 'bundle a1 a2) (bundle (lsin a1) (l* a2 (l* '(num -1) (lsin a1))))]
    [_ (error "lcos: Expecting a num or bundle instead of" a)]))

(define (lexp a)
  (match a
    [(list 'num a1) `(num ,(exp a1))]
    [(list 'bundle a1 a2) (let ((z (lexp a1)))
                            (bundle z (l* a2 z)))]
    [_ (error "lexp: Expecting a num or bundle instead of" a)]))

(define (l+ a b)
  (match* (a b)
    [((? number?) (? number?)) (+ a b)]
    [((list 'bundle a1 a2) (list 'bundle b1 b2))
     (bundle (l+ a1 b1) (l+ a2 b2))]
    [((list 'num a1) (list 'num b1))
     `(num ,(+ a1 b1))]
    [(_ _) (error "l+: Expecting bundle or num instead of" a "and" b)]))

(define (l* a b)
  (match* (a b)
    [((? number?) (? number?)) (* a b)]
    [((list 'bundle a1 a2) (list 'bundle b1 b2))
     (bundle (l* a1 b1) (l+ (l* a1 b2) (l* a2 b1)))]
    [((list 'num a1) (list 'num b1))
     `(num ,(* a1 b1))]
    [((list 'num _) (list 'bundle b1 b2)) (bundle (l* a b1) (l* a b2))]
    [((list 'bundle a1 a2) (list 'num _)) (bundle (l* b a1) (l* b a2))]
    [(_ _) (error "l*: Expecting bundle or num instead of" a "and" b)]))

(define (lift-env env)
  (let ((lifted-env
         (map (lambda (b)
                (match b
                  [(cons var val)
                   (cond [(eq? var 'num) (cons var (+ val 1))]
                         [(eq? (car val) 'closure)
                          (match val
                            [(list 'closure params body cenv)
                             (binding var `(closure ,params
                                            ,body
                                            ,(lift-env cenv)))]
                            [_ (error "lift-env: Expecting a closure instead of" val)])]
                         [(or (eq? (car val) 'bundle)
                              (eq? (car val) 'num))
                          (binding var (lift-numeric-as-const val))]
                         [#t (error "lift-env: Unknown element" binding "in env")])]
                  [_ (error "lift-env: Expecting binding instead of" binding)]))
              env)))
    (append lifted-env env)))

;;; Lift number N, i.e. (num x), C times.
;;; Example: (lift-num 4 2)
;;;          => '(bundle (bundle (num 4) (num 0)) (bundle (num 0) (num 0)))
(define (lift-num n c)
  (cond [(= c 0) `(num ,n)]
        [(> c 0) (bundle (lift-num n (- c 1))
                         (lift-num 0 (- c 1)))]))

;;; Lift numeric N, i.e. (num x) or (bundle ...), as a constant.
;;; Example: (lift-numeric-as-const '(bundle (num 3) (num 1)))
;;;          => '(bundle (bundle (num 3) (num 1)) (bundle (num 0) (num 0)))
(define (lift-numeric-as-const n)
  (bundle n (zero-out-numeric n)))


;;; Replaces all (num x) with (num 0)
;;; Example: (zero-out-numeric '(bundle (bundle (num 3) (num 2)) (bundle (num 0) (num 4))))
;;;          => '(bundle (bundle (num 0) (num 0)) (bundle (num 0) (num 0)))
(define (zero-out-numeric n)
  (match n
    [(list 'num _) `(num 0)]
    [(list 'bundle t1 t2) (list 'bundle (zero-out-numeric t1)
                                        (zero-out-numeric t2))]
    [_ (error "zero-out-numeric: Expecting num or bundle instead of" n)]))

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

(provide proto-env)
(provide eval)
(provide c-apply)
