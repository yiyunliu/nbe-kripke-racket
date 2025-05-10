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

;; (define tm-fix
;;   (let ([g (tm-abs (tm-app (tm-var 1) (tm-app (tm-var 0) (tm-var 0))))])
;;     (tm-abs (tm-app g g))))

(: tm-zero Term)
(define tm-zero tm-snd)

(: tm-suc (-> Term Term))
(define (tm-suc a) (tm-abs (tm-abs (tm-app (tm-var 1) (tm-app a (tm-var 1) (tm-var 0))))))

(: tm-add (-> Term Term Term))
(define (tm-add a b)
  (tm-abs (tm-abs
           (tm-app b (tm-var 1) (tm-app a (tm-var 1) (tm-var 0))))))

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

;; (define (tm-pnat n)
;;   (if (positive? n)
;;       `(succ ,(tm-pnat (- n 1)))
;;       'zero))

;; (define (tm-ifz a b c)
;;   `(if-zero ,a ,b ,c))

;; (define (tm-psuc a)
;;   `(succ ,a))

;; (define (tm-double m)
;;   (tm-app tm-fix
;;           (tm-abs (tm-abs (tm-ifz (tm-var 0) 'zero 'zero))) m ))

;; (define (tm-padd m n)
;;   (tm-app tm-fix
;;           (tm-abs (tm-abs (tm-abs (tm-ifz (tm-var 1) (tm-var 0) (tm-psuc (tm-app (tm-var 3) (tm-var 0) (tm-var 1))))))) m n))

(check-equal? (normalize `(app ,tm-id ,tm-id)) tm-id)
(check-equal? (normalize `(app (app (app ,tm-pair ,tm-id) ,tm-fst) ,tm-snd)) tm-fst)
(check-equal? (normalize `(app (app (app ,tm-pair ,tm-id) ,tm-fst) ,tm-fst)) tm-id)
(check-equal? (normalize (tm-app tm-snd (tm-app tm-pair tm-id tm-fst) tm-fst)) tm-fst)
(check-equal? (normalize (tm-add (tm-nat 499) (tm-nat 777))) (normalize (tm-add (tm-nat 777) (tm-nat 499))))
;; (check-equal? (normalize (tm-mult (tm-nat 3) (tm-nat 2))) (normalize (tm-nat 6)))
;; (check-equal? (normalize (tm-mult (tm-nat 11) (tm-nat 116))) (normalize (tm-nat 1276)))
;; (check η-eq? (normalize (tm-add (tm-nat 499) (tm-nat 777))) (normalize (tm-add (tm-nat 777) (tm-nat 499))))
;; (check βη-eq? (tm-mult (tm-nat 6) (tm-nat 7)) (tm-nat 42))
;; (check βη-eq? '(if-zero (succ (succ zero)) zero (succ (succ (var 0)))) (tm-pnat 3))
;; (check βη-eq? (tm-padd (tm-pnat 8) (tm-pnat 11)) (tm-pnat 19))
;; (check βη-eq? (tm-padd (tm-pnat 2) (tm-psuc (tm-var 0))) '(succ (succ (succ (var 0)))))
