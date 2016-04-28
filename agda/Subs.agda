module Subs where

open import Helpful

open import Data.Nat
open import Data.Fin
open import Data.Product
open import Data.Sum
open import Relation.Binary.PropositionalEquality
open import Data.Empty
open import Function
open import Relation.Nullary 

postulate ext : {A B : Set}{f g : A → B} →  ((x : A) → f x ≡ g x) → f ≡ g

data Ty : Set where
  Nat : Ty
  _→ₜ_ : Ty → Ty → Ty
 
-- The index for a free variable set
data VarSet : Set where
  ∅ : VarSet
  V1 : Ty → VarSet
  _∪_ : VarSet → VarSet → VarSet
  
-- The variables in a variable set
data _∈_ : VarSet → Ty → Set where
  here : {t : Ty} → V1 t ∈ t
  inL : ∀{X Y} → Var X → Var (X ∪ Y) 
  inR : ∀{X Y} → Var Y → Var (X ∪ Y) 
  
Empty : VarSet → Set
Empty V = ¬ (Var V) 


data ValG (X : Ty → Set) : Ty → Set where
  Z : ValG X Nat 
  S : ValG X Nat → ValG X Nat
  var : ∀{t} → X t → ValG X t 

Val : VarSet → Set
Val X = ValG (Var X)

valMap : {A B : Set} → (A → B) → ValG A → ValG B
valMap f Z = Z
valMap f (S v) = S (valMap f v)
valMap f (var x) = var (f x)

  
SubG : Set → Set → Set
SubG A B = A → ValG B 

_⇀_ : VarSet → VarSet → Set
_⇀_ X Y = SubG (Var X) (Var Y)

Inp : VarSet → Set
Inp X = X ⇀ ∅ 

-- Monad on Val, bind is application of substitution
_>>=_ : {X Y : Ty → Set} →  ValG X → SubG X Y → ValG Y
var x >>= σ = σ x
Z >>= σ = Z
S a >>= σ = S (a >>= σ)

-- The identity substitution
return : {X : Set} → SubG X X 
return = var 

-- Updating Variable Set
_[_//_] : (X : VarSet) → (x : Var X) → VarSet → VarSet
V1 [ here // Y ] = Y
(X1 ∪ X2) [ inL x // Y ] = (X1 [ x // Y ]) ∪ X2 
(X1 ∪ X2) [ inR x // Y ] = X1 ∪ (X2 [ x // Y ])

-- Point update substitution
_/_ : {X Y : VarSet} → (x : Var X) → Val Y → X ⇀ (X [ x // Y ]) 
_/_ here a here = a
_/_ (inL x) a (inL x') = ((x / a) x') >>= (λ y → var (inL y))
_/_ (inL x) a (inR x') = var (inR x')
_/_ (inR x) a (inL x') = var (inL x')
_/_ (inR x) a (inR x') = ((x / a) x') >>= (λ y → var (inR y))

-- Composition of substitutions (kleisli composition)
_>=>_ : {X Y Z : Set} → SubG X Y → SubG Y Z → SubG X Z
_>=>_ f g a = f a >>= g

-- The Monad laws
>>=-left : {X Y : Set} → (x : X) → (f : SubG X Y) → return x >>= f ≡ f x
>>=-left x f = refl

>>=-right : {X : Set} → (a : ValG X) → a >>= return ≡ a
>>=-right (var x) = refl
>>=-right Z = refl
>>=-right (S a) = cong S (>>=-right a) 

>>=-assoc : {X Y Z : Set} → (a : ValG X) → (f : SubG X Y) → (g : SubG Y Z) → 
             (a >>= f) >>= g ≡ a >>= (λ a → (f a >>= g))
>>=-assoc (var x) f g = refl
>>=-assoc Z f g = refl
>>=-assoc (S a) f g = cong S (>>=-assoc a f g)


-- ordering for single values
_⊑ₚ_ : ∀{X Y} → Val X → Val Y → Set 
n ⊑ₚ m = ∃ (λ σ → m ≡ n >>= σ)

-- ordering for substitutions
_⊑_ : ∀{X Y Z} → X ⇀ Y → X ⇀ Z → Set
σ ⊑ τ = ∃ (λ σ' → τ ≡ σ >=> σ')

_⊏_ : ∀{X Y Z} → X ⇀ Y → X ⇀ Z → Set
σ ⊏ τ = σ ⊑ τ × ¬ (τ ⊑ σ)



 
