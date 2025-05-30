#lang typed/racket

(define-type denv (-> V (Promise D)))
(define-type V Nonnegative-Integer)

(define-type Term (∪ 'zero
                     'nat
                     (List 'succ Term)
                     (List 'var V)
                     (List 'λ Term)
                     (List 'app Term Term)
                     (List 'ind Term Term Term)
                     (List 'Π Term Term)
                     'U))

(define-type Neu (∪ (List 'ind Neu Norm Norm)
                    (List 'var V)
                    (List 'app Neu Norm)))

(define-type Norm (∪ Neu
                     'zero
                     'nat
                     (List 'λ Norm)
                     'U
                     (List 'succ Norm)
                     (List 'Π Norm Norm)))

(: embed (-> Neu Term))
(define (embed a) a)

(: embed-norm (-> Norm Term))
(define (embed-norm a) a)

(define-type D (∪ 'zero
                  (List 'Π (Promise D) (-> (Promise D) D))
                  (List 'succ (Promise D))
                  (List 'fun (-> (Promise D) D))
                  (List 'neu D-ne)
                  'U))

(define-type D-ne (∪ (List 'app D-ne D)
                     (List 'idx V)
                     (List 'ind D-ne D (-> (Promise D) (Promise D) D))))

(: ext (All (A) (-> (-> V A) A (-> V A))))
(define (ext ρ a)
    (lambda (i)
    (if (zero? i)
        a
        (ρ (- i 1)))))

(: interp-fun (-> Term denv (-> (Promise D) D)))
(define (interp-fun a ρ)
  (λ (x) (interp a (ext ρ x))))

(: interp-fun2 (-> Term denv (-> (Promise D) (Promise D) D)))
(define (interp-fun2 a ρ)
  (λ (x y) (interp a (ext (ext ρ x) y))))

(: interp-ind (-> D (Promise D) (-> (Promise D) (Promise D) D) D))
(define (interp-ind a b c)
  (match a
    [`(neu ,u) `(neu (ind ,u ,(force b) ,c))]
    ['zero (force b)]
    [`(succ ,a) (c a (delay (interp-ind (force a) b c)))]
    [_ (error "type-error: ind")]))

(: ap (-> Term Term denv D))
(define (ap a b ρ)
  (match (interp a ρ)
    [`(fun ,f) (f (delay (interp b ρ)))]
    [`(neu ,u) `(neu (app ,u ,(interp b ρ)))]
    [_ (error "type-error: ap")]))

(: interp (-> Term denv D))
(define (interp a ρ)
  (match a
    ['U a]
    [`(Π ,A ,B) `(Π ,(delay (interp A ρ)) ,(interp-fun B ρ))]
    [`(var ,i) (force (ρ i))]
    ['zero 'zero]
    [`(succ ,a) `(succ ,(delay (interp a ρ)))]
    [`(ind ,a ,b ,c) (interp-ind
                      (interp a ρ)
                      (delay (interp b ρ))
                      (interp-fun2 c ρ))]
    [`(λ ,a) (list 'fun (interp-fun a ρ))]
    [`(app ,a ,b) (ap a b ρ)]))

(: open-dom (-> V (-> (Promise D) D) D))
(define (open-dom n f)
  (f (delay `(neu (idx ,n)))))

(: open-dom2 (-> V (-> (Promise D) (Promise D) D) D))
(define (open-dom2 n f)
  (f (delay `(neu (idx ,n))) (delay `(neu (idx ,(add1 n))))))

(: compare (-> V D D Boolean))
(define (compare n a0 a1)
  (match (list a0 a1)
    [`((Π ,A0 ,B0) (Π ,A1 ,B1))
     (and (compare n (force A0) (force A1))
          (compare (add1 n) (open-dom n B0) (open-dom n B1)))]
    ['(nat nat) #t]
    ['(U U) #t]
    ['(zero zero) #t]
    [`((succ ,a) (succ ,b)) (compare n (force a) (force b))]
    [`((fun ,f) (neu ,u)) (compare (add1 n) (open-dom n f) `(neu (app ,u (neu (idx ,n)))))]
    [`((neu ,_) (fun ,_)) (compare n a1 a0)]
    [`((neu ,a) (neu ,b)) (compare-neu n a b)]
    [_ #f]))

(: compare-neu (-> V D-ne D-ne Boolean))
(define (compare-neu n a0 a1)
  (match (list a0 a1)
    [`((idx ,i) (idx ,j)) (eqv? i j)]
    [`((app ,u0 ,v0) (app ,u1 ,v1)) (and (compare-neu n u0 u1) (compare n v0 v1))]
    [`((ind ,a0 ,b0 ,c0) (ind ,a1 ,b1 ,c1)) (and (compare-neu n a0 a1)
                                                 (compare n b0 b1)
                                                 (compare (+ n 2) (open-dom2 n c0) (open-dom2 n c1)))]
    [_ #f]))

(: reify (-> V D Norm))
(define (reify n a)
  (match a
    [`(Π ,A ,B) `(Π ,(reify n (force A)) ,(reify (+ n 1) (B (delay `(neu (idx ,n))))))]
    ['zero 'zero]
    [`(succ ,a) `(succ ,(reify n (force a)))]
    ['U a]
    [`(fun ,f) (list 'λ (reify (+ n 1) (f (delay `(neu (idx ,n))))))]
    [`(neu ,a) (reify-neu n a)]))

(: var-to-idx (-> V V V))
(define (var-to-idx s v)
  (let ([ret (- s (+ v 1))])
    (if (< ret 0)
        (error "variable to index conversion failed")
        ret)))

(: reify-neu (-> V D-ne Neu))
(define (reify-neu n a)
  (match a
    [`(ind ,a ,b ,c) (list 'ind
                           (reify-neu n a)
                           (reify n b)
                           (reify (+ n 2)
                                  (c (delay `(neu (idx ,n)))
                                     (delay `(neu (idx ,(+ 1 n)))))))]
    [`(app ,u ,v) (list 'app (reify-neu n u) (reify n v))]
    [`(idx ,i) (list 'var (var-to-idx n i))]))

(: idsub (-> V V D))
(define (idsub s i) `(neu (idx ,(var-to-idx s i))))

(: scope (-> Term V))
(define (scope a)
  (match a
    [`(Π ,A ,B) (max (scope A) (- (scope B) 2))]
    ['U 0]
    ['zero 0]
    [`(succ ,a) (scope a)]
    (`(ind ,a ,b ,c) (max (scope a) (scope b) (- (scope c) 2)))
    [`(if-zero ,a ,b ,c) (max (scope a) (scope b) (scope c))]
    [`(λ ,a) (max 0 (- (scope a) 1))]
    [`(app ,a ,b) (max (scope a) (scope b))]
    [`(var ,i) (+ i 1)]))

(: interp-with-scope (-> V Term D))
(define (interp-with-scope n a)
  (interp a (λ (x) (delay (idsub n x)))))

(: normalize (-> Term Term))
(define (normalize a)
  (let ([sa (scope a)])
    (reify sa (interp a (λ (x) (delay (idsub sa x)))))))

(: up (-> V (-> V Term) (-> V Term)))
(define (up n ρ)
  (ext (λ ([x : V]) (subst (λ (i) `(var ,(+ i n))) (ρ x))) '(var 0)))

(: subst (-> (-> V Term) Term Term))
(define (subst ρ a)
  (match a
    ['U a]
    [`(Π ,A ,B) `(Π ,(subst ρ A) ,(subst (up 1 ρ) B))]
    [`(var ,i) (ρ i)]
    [`(app ,a ,b) `(app ,(subst ρ a) ,(subst ρ b))]
    [`(λ ,a) `(λ ,(subst (up 1 ρ) a))]
    ['zero 'zero]
    [`(succ ,a) `(succ ,(subst ρ a))]
    [`(ind ,a ,b ,c) `(ind ,(subst ρ a) ,(subst ρ b) ,(subst (up 2 ρ) c))]))

(: idsub-tm (-> V Term))
(define (idsub-tm i) `(var ,i))
(: subst1 (-> Term Term Term))
(define (subst1 b a)
  (subst (ext idsub-tm b) a))

;; Coquand's algorithm but for β-normal forms
;; Subsumed by the compare function
(: η-eq? (-> Term Term Boolean))
(define (η-eq? a b)
  (match (list a b)
    ['(U U) #t ]
    [`((Π ,A0 ,B0) (Π ,A1 ,B1))
     (and (η-eq? A0 A1) (η-eq? B0 B1))]
    ['(zero zero) true]
    [`((succ ,a) (succ ,b)) (η-eq? a b)]
    [`((ind ,a ,b ,c) (ind ,a0 ,b0 ,c0))
     (and (η-eq? a a0) (η-eq? b b0) (η-eq? c c0))]
    [`((λ ,a) (λ ,b)) (η-eq? a b)]
    [`((λ ,a) ,u) (η-eq? a `(app ,(subst (λ (i) `(var ,(+ i 1))) u) (var 0)))]
    [`(,u (λ ,a)) (η-eq? `(app ,(subst (λ (i) `(var ,(+ i 1))) u) (var 0)) a)]
    [`((app ,u0 ,v0) (app ,u1 ,v1)) (and (η-eq? u0 u1) (η-eq? v0 v1))]
    [`((var ,i) (var ,j)) (eqv? i j)]
    [_ false]))

(: βη-eq? (-> Term Term Boolean))
(define (βη-eq? a b)
  (let ([n (max (scope a) (scope b))])
    (compare n (interp a (λ (x) (delay (idsub n x)))) (interp b (λ (x) (delay (idsub n x)))))))

(: β-eq? (-> Term Term Boolean))
(define (β-eq? a b)
  (equal? (normalize a) (normalize b)))

(provide reify interp normalize β-eq? Term D D-ne V βη-eq? ext subst1 subst idsub-tm)
