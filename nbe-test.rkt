#lang racket

(require rackunit "nbe.rkt")

(define-syntax tm-app
  (syntax-rules ()
    [(tm-app a) a]
    [(tm-app a b c ...)
     (tm-app `(app ,a ,b) c ...)]))

(define-syntax-rule (tm-var a) `(var ,a))
(define-syntax-rule (tm-abs a) `(λ ,a))

(define tm-id '(λ (var 0)))

(define tm-fst '(λ (λ (var 1))))

(define tm-snd '(λ (λ (var 0))))

(define tm-pair `(λ (λ (λ ,(tm-app '(var 0) '(var 2) '(var 1))))))

(define tm-y
  (let ([g (tm-abs (tm-app (tm-var 1) (tm-app (tm-var 0) (tm-var 0))))])
    (tm-abs (tm-app g g))))

(define tm-zero tm-snd)

(define (tm-suc a) (tm-abs (tm-abs (tm-app (tm-var 1) (tm-app a (tm-var 1) (tm-var 0))))))

(define (tm-add a b)
  (tm-abs (tm-abs
           (tm-app b (tm-var 1) (tm-app a (tm-var 1) (tm-var 0))))))

(define (tm-nat n)
  (if (positive? n)
      (tm-suc (tm-nat (- n 1)))
      tm-zero))

(check-equal? (normalize `(app ,tm-id ,tm-id)) tm-id)
(check-equal? (normalize `(app (app (app ,tm-pair ,tm-id) ,tm-fst) ,tm-snd)) tm-fst)
(check-equal? (normalize `(app (app (app ,tm-pair ,tm-id) ,tm-fst) ,tm-fst)) tm-id)
(check-equal? (normalize (tm-app tm-snd (tm-app tm-pair tm-id tm-fst) tm-fst)) tm-fst)
(check-equal? (normalize (tm-add (tm-nat 100) (tm-nat 40))) (normalize (tm-add (tm-nat 40) (tm-nat 100))))
