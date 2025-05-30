#lang typed/racket

(require "nbe.rkt")

(define-type context (Immutable-HashTable V Term))

(: context-len (-> context V))
(define (context-len Γ)
  (hash-count Γ))

(: context-ext (-> context Term context))
(define (context-ext Γ A)
  (hash-set Γ (context-len Γ) A))

(: context-get (-> context V Term))
(define (context-get Γ i)
  (if (< i (context-len Γ))
      (subst (λ (j) `(var ,(+ i j 1))) (hash-ref Γ (- (context-len Γ) (add1 i))))
      (error "context-get: out of bound")))

(: context-empty context)
(define context-empty (hash))

(: synth (-> context Term Term))
(define (synth Γ a)
  (match a
    [`(var ,i) (context-get Γ i)]
    [`(app ,a ,b)
     (let ([A (synth Γ a)]
           [D (interp-with-scope (context-len Γ) A)]))]))

(: check (-> context Term D Boolean))
(define (check Γ a A)
  (error "unimplemented"))
