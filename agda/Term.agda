module Term where

open import Helpful
open import Data.Nat hiding (_≟_) hiding (_*_)
open import Data.Fin hiding (_+_) 
open import Data.Unit hiding (_≟_)
open import Data.Empty
open import Relation.Binary.PropositionalEquality hiding ([_])
open import Data.Product
open import Relation.Nullary

data Ty : Set where
  Nat : Ty
  _→ₜ_ : Ty → Ty → Ty

data Cxt : Set where
  ε : Cxt
  _,_ : Cxt → Ty → Cxt
  
data _∈_ (t : Ty) : Cxt → Set where
  here : ∀{Γ} → t ∈ Γ , t
  there : ∀{u Γ} → t ∈ Γ → t ∈ Γ , u
infix 3 _∈_ 

data Term (Γ : Cxt) : Ty → Set where
  Z : Term Γ Nat 
  S : Term Γ Nat → Term Γ Nat 
  bot : ∀{t} → Term Γ t
  ƛ : ∀{u t} → (e : Term (Γ , u) t) → Term Γ (u →ₜ t)
  case : ∀{t} → (e : Term Γ Nat) → (e' : Term Γ t) → (e'' : Term Γ (Nat →ₜ t)) → Term Γ t

  var : ∀{t} → (v : t ∈ Γ) → Term Γ t
  app : ∀{u t} → Term Γ (u →ₜ t) → Term Γ u → Term Γ t

data Val (Γ : Cxt) : Ty → Set where
  Z : Val Γ Nat 
  S : (Val Γ Nat) → Val Γ Nat 
  bot : ∀{t} → Val Γ t
  ƛ : ∀{u t} → (e : Term (Γ , u) t) → Val Γ (u →ₜ t)
 
data Inp : Cxt → Set where
  ε : Inp ε 
  _,_ : ∀{t Γ} → Inp Γ → Term Γ t → Inp (Γ , t)

Ren : Cxt → Cxt → Set
Ren Γ Γ′ = ∀ {τ} → τ ∈ Γ → τ ∈ Γ′

extend' : ∀ {Γ Γ′ τ′} → Ren Γ Γ′ → Ren (Γ , τ′) (Γ′  , τ′)
extend' f here = here
extend' f (there x) = there (f x)

_*'_ : ∀ {Γ Γ′ : Cxt} {τ} → Ren Γ Γ′ → Term Γ τ → Term Γ′ τ
r *' Z = Z
r *' S e = S (r *' e)
r *' bot = bot
r *' ƛ e = ƛ (extend' r *' e)
r *' case e e₁ e₂ = case (r *' e) (r *' e₁) (r *' e₂)
r *' var v = var (r v)
r *' app e e₁ = app (r *' e) (r *' e₁)

-- A substitution from Γ to Γ′.
Sub : Cxt → Cxt → Set
Sub Γ Γ′ = ∀ {τ} → τ ∈ Γ → Term Γ′ τ

extend : ∀ {Γ Γ′ t} → Sub Γ Γ′ → Sub (Γ , t) (Γ′ , t)
extend θ here = var here
extend θ (there x) = there *' θ x

_*_ : ∀ {Γ Γ′ : Cxt} {τ} → Sub Γ Γ′ → Term Γ τ → Term Γ′ τ
f * Z = Z
f * S e = S (f * e)
f * bot = bot
f * ƛ e = ƛ (extend f * e)
f * case e e₁ e₂ = case (f * e) (f * e₁) (f * e₂)
f * var v = f v
f * app e e₁ = app (f * e) (f * e₁)

sub : ∀{t Γ} → Term Γ t → Sub (Γ , t) Γ
sub e here = e
sub e (there v) = var v

_⟨_⟩ : ∀{Γ u t} → Term (Γ , u) t → Term Γ u → Term Γ t 
f ⟨ e ⟩ = sub e * f

data _↦_ {Γ : Cxt}{t : Ty} : Term Γ t → Term Γ t → Set where
  caseSubj : ∀{e e'} → e ↦ e' → ∀{e₁ e₂}  
           → case e e₁ e₂ ↦ case e' e₁ e₂
  caseZ : ∀{e₁ e₂} → case Z e₁ e₂ ↦ e₁
  caseS : ∀{eₛ e₁ e₂} → case (S eₛ) e₁ e₂ ↦ app e₂ eₛ
  casebot : ∀{e₁ e₂} → case bot e₁ e₂ ↦ bot
  appL : ∀{u}{e e' : Term Γ (u →ₜ t)} → e ↦ e' → ∀{e''}  
           → app e e'' ↦ app e' e'' 
  app : ∀{u}{e : Term (Γ , u) t}{e' : Term Γ u} → app (ƛ e) e' ↦ e ⟨ e' ⟩
  appbot : ∀{u}{e' : Term Γ u} → app bot e' ↦ bot
  
data _⇓ {Γ : Cxt} : {t : Ty} → Term Γ t → Set where
  Z : Z ⇓ 
  S : ∀{e} → (e ⇓) → S e ⇓
  bot : ∀{t} → _⇓ {t = t} bot 
  ƛ : ∀{u t}{e : Term (Γ , u) t} → ƛ e ⇓ 
  red : ∀{t e e'} → _↦_ {t = t} e e' → e' ⇓ → e ⇓ 
  
evalVal : ∀{Γ t} {e : Term Γ t} → e ⇓ → Val Γ t
evalVal Z = Z
evalVal (S x) = S (evalVal x)
evalVal bot = bot
evalVal {e = ƛ e} ƛ = ƛ e
evalVal (red x re) = evalVal re


WN' : ∀ t → Term ε t → Set
WN : ∀ t → Term ε t → Set

WN t e = e ⇓ × WN' t e

WN' Nat e = ⊤
WN' (t₁ →ₜ t₂) e = (e' : Term ε t₁) → WN t₁ e' → WN t₂ (app e e')

----WNtoEval : ∀{t V}{Γ : Cxt V}{e : Term Γ t} →  WN e → e ⇓
----WNtoEval (d , proj₂) = d
--

subInp : ∀{Γ} → Inp Γ → Sub Γ ε
subInp ε ()
subInp (es , e) here = subInp es * e
subInp (es , e) (there v) = subInp es v

_[_] : ∀{t Γ} → Term Γ t → Inp Γ → Term ε t
e [ σ ] = subInp σ * e

SubRefl : ∀{Γ} → Sub Γ Γ → Set
SubRefl {Γ} f = ∀ {t} → (v : t ∈ Γ) → f v ≡ var v

sub-refl-ε : (f : Sub ε ε) → SubRefl f
sub-refl-ε f ()

extendRef : ∀{Γ t}{f : Sub Γ Γ} → 
          SubRefl f → SubRefl (extend {t = t} f)
extendRef eq here = refl
extendRef eq (there v) = cong (λ x → there *' x) (eq v)

sub-refl : ∀{Γ t} → (e : Term Γ t) → (f : Sub Γ Γ) → SubRefl f → f * e ≡ e
sub-refl Z f r = refl
sub-refl (S e) f r = cong S (sub-refl e f r)
sub-refl bot f r = refl
sub-refl (ƛ e) f r = cong ƛ (sub-refl e (extend f) (extendRef r))
sub-refl (case e e₁ e₂) f r = cong₃ case (sub-refl e f r) (sub-refl e₁ f r) (sub-refl e₂ f r)
sub-refl (var v) f r = r v
sub-refl (app e e₁) f r = cong₂ app (sub-refl e f r) (sub-refl e₁ f r)

push-SN : ∀{t}{e e' : Term ε t} → e ↦ e' → WN t e → WN t e'
push-SN r (r* , wn') = {!red!} , {!!}

pull-SN : ∀{t}{e e' : Term ε t} → e ↦ e' → WN t e' → WN t e
pull-SN {Nat} r (r* , wn') = red r r* , tt
pull-SN {t →ₜ t₁}{e}{e'} r (r* , wn') = red r r* , (λ e'' x → pull-SN (appL r) (wn' e'' x)) 

Inp-SN : (Γ : Cxt) → Inp Γ → Set 
Inp-SN ε ε = ⊤
Inp-SN (Γ , t) (σ , e) = Inp-SN Γ σ × (Inp-SN Γ σ → WN t (e [ σ ])) 

SN : ∀ {Γ} t {σ : Inp Γ} → (e : Term Γ t) → Inp-SN Γ σ → WN t (e [ σ ])
SN .Nat Z is = Z , tt
SN .Nat (S e) is with SN Nat e is 
...| r , a = S r , tt
SN Nat bot is = bot , tt
SN (t →ₜ t₁) {σ} bot is = {!!} -- bot , (λ e' _ → let z = SN {ε} t₁ {ε} (app (bot [ σ ]) e') tt in 
--         subst (λ x → WN t₁ x) (sub-refl (app bot e') (subInp ε) (sub-refl-ε (subInp ε))) z)
SN (t →ₜ t₁) {σ} (ƛ e) is = {!!} -- ƛ , λ e' wn → subst (λ x → WN t₁ x) (sub-refl (app (ƛ e [ σ ]) e') (subInp ε) (sub-refl-ε (subInp ε))) (SN t₁ (app (ƛ e [ σ ]) e') tt)
SN Nat (case e e₁ e₂) is = {!!} , {!!}
SN (t →ₜ t₁) (case e e₁ e₂) is = {!!}
SN {ε} t (var ()) is
SN {Γ , .t} t {σ , a} (var here) (proj₁ , proj₂) = proj₂ proj₁
SN {Γ , x} t {σ , a} (var (there v)) (is , i) = SN t (var v) is
SN t {σ} (app {u}{.t} e e₁) is with SN (u →ₜ t) e is | SN u e₁ is 
...| r , wn | wn'  = wn (e₁ [ σ ]) wn' 
