module Partial where

open import Data.Unit

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

Term : Set₁
Term = Cxt → Ty → Set

Eval : Term → Set
Eval T = ∀{X τ} → T X τ → T X τ 

data Obs (X : Ty → Set) : Ty → Set where
  Z : Obs X Nat 
  S : Obs X Nat → Obs X Nat
  oth : ∀{t} → X t → Obs X t 

Observe : Term → Set
Observe T = ∀{X t} → T X t → Obs (λ _ -> ⊤) t

record Lang : Set₁ where
  field
    T : Term 
    eval : Eval T
    ⌜_⌝ : Observe T
    
open Lang

data ObsOrd {X Y : Ty → Set}(LE : ∀{t} → X t → Obs Y t → Set) : {t : Ty} → Obs X t → Obs Y t → Set where
  Z≤ : ObsOrd LE Z Z
  S≤ : ∀{e e'} → ObsOrd LE e e' → ObsOrd LE (S e) (S e')
  susp≤ : ∀{t}{x : X t}{e : Obs Y t} → LE x e → ObsOrd LE (oth x) e
  

