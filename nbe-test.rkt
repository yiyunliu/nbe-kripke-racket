#lang typed/racket

(require typed/rackunit "nbe.rkt")

(define-syntax tm-app
  (syntax-rules ()
    [(tm-app a) a]
    [(tm-app a b c ...)
     (tm-app `(app ,a ,b) c ...)]))

(define-syntax-rule (tm-var a) `(var ,a))
(define-syntax-rule (tm-abs a) `(λ ,a))

(: tm-id Term)
(define tm-id '(λ (var 0)))

(: tm-fst Term)
(define tm-fst '(λ (λ (var 1))))

(: tm-snd Term)
(define tm-snd '(λ (λ (var 0))))

(: tm-pair Term)
(define tm-pair `(λ (λ (λ ,(tm-app '(var 0) '(var 2) '(var 1))))))

(: tm-fix Term)
(define tm-fix
  (let ([g (tm-abs (tm-app (tm-var 1) (tm-app (tm-var 0) (tm-var 0))))])
    (tm-abs (tm-app g g))))

(: tm-zero Term)
(define tm-zero tm-snd)

(: tm-suc (-> Term Term))
(define (tm-suc a) (tm-abs (tm-abs (tm-app (tm-var 1) (tm-app a (tm-var 1) (tm-var 0))))))

(: tm-add (-> Term Term Term))
(define (tm-add a b)
  (tm-abs (tm-abs
           (tm-app b (tm-var 1) (tm-app a (tm-var 1) (tm-var 0))))))

(: tm-ind (-> Term Term Term Term))
(define (tm-ind a b c)
  `(ind ,a ,b ,c))

(: tm-padd (-> Term Term Term))
(define (tm-padd a b)
  (tm-app (tm-ind a tm-id (tm-abs (tm-psuc (tm-app (tm-var 1) (tm-var 0))))) b))

(: tm-compose (-> Term Term Term))
(define (tm-compose a b)
  (tm-abs (tm-app a (tm-app b (tm-var 0)))))

(: tm-mult (-> Term Term Term))
(define (tm-mult a b) (tm-compose a b))

(: tm-nat (-> V Term))
(define (tm-nat n)
  (if (positive? n)
      (tm-suc (tm-nat (- n 1)))
      tm-zero))

(: tm-const Term)
(define tm-const tm-fst)

(: tm-loop Term)
(define tm-loop
  (let ([g (tm-abs (tm-app (tm-var 0) (tm-var 0)))])
    (tm-app g g)))

(: tm-nat-to-pnat Term)
(define tm-nat-to-pnat
  (tm-abs (tm-app (tm-var 0) (tm-abs '(succ (var 0))) 'zero)))

(: tm-pnat (-> V Term))
(define (tm-pnat n)
  (if (positive? n)
      `(succ ,(tm-pnat (- n 1)))
      'zero))

(: tm-psuc (-> Term Term))
(define (tm-psuc a)
  `(succ ,a))

(check-equal? (normalize `(app ,tm-id ,tm-id)) tm-id)
(check-equal? (normalize `(app (app (app ,tm-pair ,tm-id) ,tm-fst) ,tm-snd)) tm-fst)
(check-equal? (normalize `(app (app (app ,tm-pair ,tm-id) ,tm-fst) ,tm-fst)) tm-id)
(check-equal? (normalize (tm-app tm-snd (tm-app tm-pair tm-id tm-fst) tm-fst)) tm-fst)
(check-equal? (normalize (tm-add (tm-nat 499) (tm-nat 777))) (normalize (tm-add (tm-nat 777) (tm-nat 499))))
(check-equal? (normalize (tm-app tm-const (tm-nat 0) tm-loop)) (normalize (tm-nat 0)))
(check-equal? (normalize (tm-app tm-nat-to-pnat (tm-nat 10))) (tm-pnat 10))
(check-equal? (normalize `(ind ,(tm-pnat 3) ,(tm-pnat 0) (var 1))) (tm-pnat 2))
(check-equal? (normalize `(ind ,(tm-pnat 3) ,tm-loop (var 1))) (tm-pnat 2))
(check-equal? (normalize (tm-padd (tm-pnat 10000) (tm-pnat 2000)))
              (tm-pnat 12000))
