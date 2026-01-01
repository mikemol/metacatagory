{-# OPTIONS --without-K #-}

-- | Functor instance for Plan.CIM.Ambiguity using a Funext adapter.

module Infrastructure.Functor.Instances.Ambiguity where

open import Agda.Primitive using (Level; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Unit using (⊤; tt)

open import Infrastructure.Functor.Interface
open import Infrastructure.Functor.Instances.FunctionCategory using (FunctionCategory; Lift; lift)
open import Infrastructure.Functor.Adapters.Funext
open import Infrastructure.Equality using (cong; sym)
open import Plan.CIM.Ambiguity using (Ambiguity; mapAmbiguity; mapOption; WeightedOption; determinate; superposition; conflict)
open import Infrastructure.Axiom.Adequacy using (PathAlgebra)
open import Infrastructure.Axiom.Instance using (AxiomInstance; FramedFace)
open import Infrastructure.Axiom.Face using (Face)

mapOption-id : ∀ {ℓ} {A : Set ℓ} (xs : List (WeightedOption A)) → mapOption (λ y → y) xs ≡ xs
mapOption-id [] = refl
mapOption-id (rec ∷ rs) =
  let rec' = record { value = WeightedOption.value rec
                    ; weight = WeightedOption.weight rec
                    ; provenance = WeightedOption.provenance rec }
  in cong (λ ys → rec' ∷ ys) (mapOption-id rs)

mapOption-compose : ∀ {ℓ} {A B C : Set ℓ} (g : B → C) (f : A → B) (xs : List (WeightedOption A)) →
  mapOption g (mapOption f xs) ≡ mapOption (λ x → g (f x)) xs
mapOption-compose g f [] = refl
mapOption-compose g f (rec ∷ rs) =
  let rec' = record { value = g (f (WeightedOption.value rec))
                    ; weight = WeightedOption.weight rec
                    ; provenance = WeightedOption.provenance rec }
  in cong (λ ys → rec' ∷ ys) (mapOption-compose g f rs)

mapAmbiguity-id-pt : ∀ {ℓ} {A : Set ℓ} (x : Ambiguity A) → mapAmbiguity (λ y → y) x ≡ x
mapAmbiguity-id-pt (determinate _)    = refl
mapAmbiguity-id-pt (superposition xs) = cong superposition (mapOption-id xs)
mapAmbiguity-id-pt (conflict _)       = refl

mapAmbiguity-comp-pt : ∀ {ℓ} {A B C : Set ℓ} (g : B → C) (f : A → B) (x : Ambiguity A) →
  mapAmbiguity (λ y → g (f y)) x ≡ mapAmbiguity g (mapAmbiguity f x)
mapAmbiguity-comp-pt g f (determinate _)    = refl
mapAmbiguity-comp-pt g f (superposition xs) = cong superposition (sym (mapOption-compose g f xs))
mapAmbiguity-comp-pt g f (conflict _)       = refl

AmbiguityFunctor : (∀ {ℓ} {A B : Set ℓ} → Funext (Ambiguity A) (Ambiguity B)) →
  ∀ {ℓ} → FunctorInstance (FunctionCategory {ℓ}) (FunctionCategory {ℓ})
FunctorInstance.objMap (AmbiguityFunctor fe) A = Ambiguity A
FunctorInstance.map    (AmbiguityFunctor fe) f = lift (mapAmbiguity (Lift.lower f))
FunctorInstance.map-id (AmbiguityFunctor fe) {A} =
  cong lift (pointwise→≡ (fe {A = A} {B = A}) mapAmbiguity-id-pt)
FunctorInstance.map-compose (AmbiguityFunctor fe) {A} {B} {C} g f =
  cong lift (pointwise→≡ (fe {A = A} {B = C}) (mapAmbiguity-comp-pt (Lift.lower g) (Lift.lower f)))

------------------------------------------------------------------------
-- Optional adequacy kit for Ambiguity functor (parameterized by Funext)
------------------------------------------------------------------------

module AmbiguityAdequacy {ℓ} (fe : ∀ {A B : Set ℓ} → Funext (Ambiguity A) (Ambiguity B)) where
  open Infrastructure.Functor.Interface
  open import Infrastructure.Axiom.Adequacy
  open import Infrastructure.Axiom.Instance

  ambiguityPathAlgebra : PathAlgebra {ℓV = lsuc ℓ} {ℓP = lsuc ℓ} (Set ℓ)
  PathAlgebra.Path ambiguityPathAlgebra A B = Lift {ℓ = ℓ} {ℓ' = lsuc ℓ} (Ambiguity A → Ambiguity B)
  PathAlgebra._++_ ambiguityPathAlgebra {A} {B} {C} f g =
    lift (λ x → Lift.lower g (Lift.lower f x))
  PathAlgebra.++-assoc ambiguityPathAlgebra f g h = refl
  PathAlgebra.id ambiguityPathAlgebra = lift (λ x → x)
  PathAlgebra.id-left ambiguityPathAlgebra p = refl
  PathAlgebra.id-right ambiguityPathAlgebra p = refl

  record AmbiguityKit : Set (lsuc ℓ) where
    field
      A B : Set ℓ
      f   : Lift {ℓ = ℓ} {ℓ' = lsuc ℓ} (Ambiguity A → Ambiguity B)

  ambiguityFace : AmbiguityKit → FramedFace ambiguityPathAlgebra
  FramedFace.a    (ambiguityFace k) = AmbiguityKit.A k
  FramedFace.b    (ambiguityFace k) = AmbiguityKit.B k
  FramedFace.face (ambiguityFace k) = record { lhs = AmbiguityKit.f k ; rhs = AmbiguityKit.f k }

  ambiguityAxiomInstance : AxiomInstance ambiguityPathAlgebra
  AxiomInstance.Kit   ambiguityAxiomInstance = AmbiguityKit
  AxiomInstance.face  ambiguityAxiomInstance = ambiguityFace
  AxiomInstance.solve ambiguityAxiomInstance _ = refl
