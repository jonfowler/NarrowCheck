module Helpful where

open import Relation.Binary.PropositionalEquality
open import Relation.Nullary

cong₃ : ∀ {a b c d} {A : Set a} {B : Set b} {C : Set c} {D : Set d}
        (f : A → B → C → D) {x y u v m n} → x ≡ y → u ≡ v → m ≡ n → f x u m ≡ f y v n
cong₃ f refl refl refl = refl

subst₃ : ∀ {A B C : Set}(P : A → B → C → Set)
         {x₁ x₂ y₁ y₂ z₁ z₂} → x₁ ≡ x₂ → y₁ ≡ y₂ → z₁ ≡ z₂ → P x₁ y₁ z₁ → P x₂ y₂ z₂
subst₃ P refl refl refl a = a

_≠_ : {A : Set} → A → A → Set
a ≠ b = ¬ (a ≡ b)

≠sym : {A : Set}{a b : A} → a ≠ b → b ≠ a
≠sym eq x = eq (sym x)

