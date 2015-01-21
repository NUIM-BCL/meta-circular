#lang racket

;;; To run the test suite, execute 'racket camogie.rkt'.

(require rackunit "camogie.rkt")

(check-equal? (eval '(D (lambda (x) (* x (D (lambda (y) (+ x y)) (num 1)))) (num 1)) proto-env)
              '(num 1))

(check-equal? (eval '(D (lambda (x) x) (num 7)) proto-env)
              '(num 1))

(check-equal? (eval '(D (lambda (x) (num 3)) (num 7)) proto-env)
              '(num 0))

(check-equal? (eval '(D (lambda (x) (+ x (num 1))) (num 7)) proto-env)
              '(num 1))

(check-equal? (eval '(D (lambda (x) (* x (D (lambda (y) (+ (* (num 2) x) y)) (num 3)))) (num 7))
                    proto-env)
              '(num 1))

(check-equal? (eval '(D (lambda (x) (* x (D (lambda (y) (* x y)) (num 3)))) (num 7)) proto-env)
              '(num 14))

(check-equal? (eval '(D (lambda (y) (D (lambda (x) (* x (* x y))) (* y (num 2)))) (num 2)) proto-env)
              '(num 16))
