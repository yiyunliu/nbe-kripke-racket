#lang typed/racket
;; Grammar (Λ)
;; t := λ t | app t t | i

(define-type denv (-> V (Promise D)))
(define-type V Nonnegative-Integer)
(define-type Term (∪ 'zero
                     (List 'succ Term)
                     (List 'var V)
                     (List 'λ Term)
                     (List 'app Term Term)
                     (List 'ind Term Term Term)))

(define-type D (∪ 'zero
                  (List 'succ (Promise D))
                  (List 'fun (-> (Promise D) D))
                  (List 'fun2 (-> (Promise D) (Promise D) D))
                  (List 'neu D-ne)
                  ;; (List 'ind (-> (Promise D) (Promise D) D))
                  ))

(define-type D-ne (∪ (List 'app D-ne D) (List 'idx V)))

(: ext (-> denv (Promise D) denv))
(define (ext ρ a)
    (lambda (i)
    (if (zero? i)
        a
        (ρ (- i 1)))))

(define-syntax-rule (ap a b)
  (match a
    [`(fun ,f) (f (delay b))]
    [`(neu ,u) `(neu (app ,u ,b))]))

(: interp-fun (-> Term denv D))
(define (interp-fun a ρ)
  (list 'fun (λ (x) (interp a (ext ρ x)))))

(: interp-ind (-> D (Promise D) (Promise D) denv D))
(define (interp-ind a b c ρ)
  (match a
    [_ (error "unimplemented")]))

(: interp (-> Term denv D))
(define (interp a ρ)
  (match a
    [`(var ,i) (force (ρ i))]
    ['zero 'zero]
    [`(succ ,a) `(succ ,(delay (interp a ρ)))]
    [`(ind ,a ,b ,c) (interp-ind
                      (interp a ρ)
                      (delay (interp b ρ))
                      (delay (interp c ρ)) ρ)]
    [`(λ ,a) (interp-fun a ρ)]
    [`(app ,a ,b) (ap (interp a ρ) (interp b ρ))]))

;; Domain
;; D := neu D_ne | fun [(var -> var) -> D → D]
;; D_ne := var i | app D_ne D


;; (: interp (-> Term (-> Term)))

;; (define (interp a ρ)
;;   (delay (match a
;;     [`(var ,i) (force (ρ i))]
;;     ['zero 'zero]
;;     [`(succ ,a) `(succ ,(interp a ρ))]
;;     [`(if-zero ,a ,b ,c) (ifz (interp a ρ) (interp b ρ) (interp-fun c ρ))]
;;     [`(λ ,a) (interp-fun a ρ)]
;;     [`(app ,a ,b) (ap (interp a ρ) (interp b ρ))])))

(: reify (-> V D Term))
(define (reify n a)
  (match a
    ['zero 'zero]
    [`(succ ,a) `(succ ,(reify n (force a)))]
    [`(fun ,f) (list 'λ (reify (+ n 1) (f (delay `(neu (idx ,n))))))]
    [`(neu ,a) (reify-neu n a)]))

;; (define (extract-body a)
;;   (match a
;;     [`(λ ,a) a]
;;     [_ (error "reify-neu: not reifiable")]))

(: reify-neu (-> V D-ne Term))
(define (reify-neu n a)
  (match a
    ;; [`(if-zero ,a ,b ,c) (list 'if (reify-neu n a) (reify n b) (extract-body (reify n c)))]
    [`(app ,u ,v) (list 'app (reify-neu n u) (reify n v))]
    [`(idx ,i) (list 'var (max 0 (- n (+ i 1))))]))

(: idsub (-> V V D))
(define (idsub s i) `(neu (idx ,(max 0 (- s (+ i 1))))))

(: scope (-> Term V))
(define (scope a)
  (match a
    ['zero 0]
    [`(succ ,a) (scope a)]
    (`(ind ,a ,b ,c) (max (scope a) (scope b) (scope c)))
    [`(if-zero ,a ,b ,c) (max (scope a) (scope b) (scope c))]
    [`(λ ,a) (max 0 (- (scope a) 1))]
    [`(app ,a ,b) (max (scope a) (scope b))]
    [`(var ,i) (+ i 1)]))

(: normalize (-> Term Term))
(define (normalize a)
  (let ([sa (scope a)])
    (reify sa (interp a (λ (x) (delay (idsub sa x)))))))

;; (define (subst ρ a)
;;   (match a
;;     [`(var ,i) (ρ i)]
;;     [`(app ,a ,b) `(app ,(subst ρ a) ,(subst ρ b))]
;;     [`(λ ,a) `(λ ,(subst (ext (compose (curry subst (λ (i) `(var ,(+ i 1)))) ρ)
;;                               '(var 0)) a))]))

;; (define (idsub-tm i) `(var ,i))
;; (define (subst1 b a)
;;   (subst (ext idsub-tm b) a))

;; (define (eval-tm a)
;;   (match a
;;     [(list 'var _) a]
;;     [(list 'λ a) `(λ ,(eval-tm a))]
;;     [(list 'app a b)
;;      (match (eval-tm a)
;;        [(list 'λ a) (eval-tm (subst1 b a))]
;;        [v `(app ,v ,(eval-tm b))])]))

;; (define (eval-tm-strict a)
;;   (match a
;;     [(list 'var _) a]
;;     [(list 'λ a) `(λ ,(eval-tm-strict a))]
;;     [(list 'app a b)
;;      (match (eval-tm-strict a)
;;        [(list 'λ a) (eval-tm-strict (subst1 (eval-tm-strict b) a))]
;;        [v `(app ,v ,(eval-tm-strict b))])]))

;; ;; Coquand's algorithm but for β-normal forms
;; (: η-eq? (-> Term Term Boolean))
;; (define (η-eq? a b)
;;   (match (list a b)
;;     ['(zero zero) true]
;;     [`((succ ,a) (succ ,b)) (η-eq? a b)]
;;     [`((if-zero ,a ,b ,c) (if-zero ,a0 ,b0 ,c0))
;;      (and (η-eq? a a0) (η-eq? b b0) (η-eq? c c0))]
;;     [`((λ ,a) (λ ,b)) (η-eq? a b)]
;;     [`((λ ,a) ,u) (η-eq? a `(app ,(subst (λ (i) `(var ,(+ i 1))) u) (var 0)))]
;;     [`(,u (λ ,a)) (η-eq? `(app ,(subst (λ (i) `(var ,(+ i 1))) u) (var 0)) a)]
;;     [`((app ,u0 ,v0) (app ,u1 ,v1)) (and (η-eq? u0 u1) (η-eq? v0 v1))]
;;     [`((var ,i) (var ,j)) (eqv? i j)]
;;     [_ false]))


;; (define (βη-eq? a b)
;;   (η-eq? (normalize a) (normalize b)))

(: β-eq? (-> Term Term Boolean))
(define (β-eq? a b)
  (equal? (normalize a) (normalize b)))

(provide reify interp normalize β-eq? Term D V)
