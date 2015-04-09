#lang racket

;;; To run the test suite, execute 'racket camogie-test.rkt'.

(require rackunit "camogie.rkt")

(check-equal? (eval '(D (lambda (x) (* x (D (lambda (y) (+ x y)) 1))) 1))
              '1)

(check-equal? (eval '(D (lambda (x) x) 7))
              '1)

(check-equal? (eval '(D (lambda (x) 3) 7))
              '0)

(check-equal? (eval '(D (lambda (x) (+ x 1)) 7))
              '1)

(check-equal? (eval '(D (lambda (x) (* x (D (lambda (y) (+ (* 2 x) y)) 3))) 7))
              '1)

(check-equal? (eval '(D (lambda (x) (* x (D (lambda (y) (* x y)) 3))) 7))
              14)

(check-equal? (eval '(D (lambda (y) (D (lambda (x) (* x (* x y))) (* y 2))) 2))
              16)
