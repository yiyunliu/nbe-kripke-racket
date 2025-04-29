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

(define (tm-compose a b)
  (tm-abs (tm-app a (tm-app b (tm-var 0)))))

(define (tm-mult a b) (tm-compose a b))

(define (tm-nat n)
  (if (positive? n)
      (tm-suc (tm-nat (- n 1)))
      tm-zero))

(check-equal? (normalize `(app ,tm-id ,tm-id)) tm-id)
(check-equal? (normalize `(app (app (app ,tm-pair ,tm-id) ,tm-fst) ,tm-snd)) tm-fst)
(check-equal? (normalize `(app (app (app ,tm-pair ,tm-id) ,tm-fst) ,tm-fst)) tm-id)
(check-equal? (normalize (tm-app tm-snd (tm-app tm-pair tm-id tm-fst) tm-fst)) tm-fst)
(check-equal? (normalize (tm-add (tm-nat 499) (tm-nat 777))) (normalize (tm-add (tm-nat 777) (tm-nat 499))))
(check-equal? (normalize (tm-mult (tm-nat 3) (tm-nat 2))) (normalize (tm-nat 6)))
(check-equal? (normalize (tm-mult (tm-nat 11) (tm-nat 116))) (normalize (tm-nat 1276)))
(check η-eq? (normalize (tm-add (tm-nat 499) (tm-nat 777))) (normalize (tm-add (tm-nat 777) (tm-nat 499))))
(check βη-eq? (tm-mult (tm-nat 888) (tm-nat 999)) (tm-nat 887112))
(check β-eq? (tm-mult (tm-nat 888) (tm-nat 999)) (tm-nat 887112))
