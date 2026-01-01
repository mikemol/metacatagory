{-# OPTIONS --without-K #-}

-- | Functor instance for transformation systems using their path structure.
-- We build a lightweight category where the single object carries morphisms
-- given by `Path Sys`, with composition as path concatenation.

module Infrastructure.Functor.Instances.TransformationSystem where

open import Agda.Primitive using (Level)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Plan.CIM.Utility using (TransformationSystem; Path; refl-path; trans-step)
open import Infrastructure.Functor.Interface using (CategoryLike; FunctorInstance)
open import Infrastructure.Functor.Instances.Trivial
open import Infrastructure.Axiom.Adequacy using (PathAlgebra)
open import Infrastructure.Axiom.Instance using (AxiomInstance; FramedFace)
open import Infrastructure.Axiom.Face using (Face)

-- | Singleton carrier used to package transformation paths as morphisms.
record One {ℓ} : Set ℓ where
  constructor tt

------------------------------------------------------------------------
-- Path category for a transformation system
------------------------------------------------------------------------

_⊕_ : ∀ {ℓ A B} {Sys : TransformationSystem {ℓ} A B} →
      Path Sys → Path Sys → Path Sys
_⊕_ refl-path ys = ys
_⊕_ (trans-step s rest) ys = trans-step s (_⊕_ rest ys)

⊕-id-left : ∀ {ℓ A B} {Sys : TransformationSystem {ℓ} A B} (p : Path Sys) →
            _⊕_ refl-path p ≡ p
⊕-id-left p = refl

⊕-id-right : ∀ {ℓ A B} {Sys : TransformationSystem {ℓ} A B} (p : Path Sys) →
             _⊕_ p refl-path ≡ p
⊕-id-right refl-path = refl
⊕-id-right (trans-step s rest) rewrite ⊕-id-right rest = refl

⊕-assoc : ∀ {ℓ A B} {Sys : TransformationSystem {ℓ} A B}
          (p q r : Path Sys) →
          _⊕_ (_⊕_ p q) r ≡ _⊕_ p (_⊕_ q r)
⊕-assoc refl-path q r = refl
⊕-assoc (trans-step s rest) q r rewrite ⊕-assoc rest q r = refl

pathCategory : ∀ {ℓ A B} (Sys : TransformationSystem {ℓ} A B) → CategoryLike (One {ℓ})
pathCategory Sys = record
  { Hom      = λ _ _ → Path Sys
  ; id       = refl-path
  ; _∘_      = λ f g → _⊕_ g f  -- right-to-left composition
  ; id-left  = ⊕-id-right
  ; id-right = ⊕-id-left
  ; assoc    = λ h g f → ⊕-assoc f g h
  }

------------------------------------------------------------------------
-- Identity functor on the path category (for reuse in adapters)
------------------------------------------------------------------------

pathIdentity : ∀ {ℓ A B} (Sys : TransformationSystem {ℓ} A B) →
               FunctorInstance (pathCategory Sys) (pathCategory Sys)
pathIdentity Sys = identity
  where
    open IdentityFunctor (pathCategory Sys)

------------------------------------------------------------------------
-- Optional adequacy kit for the transformation path functor
------------------------------------------------------------------------

module TransformationPathAdequacy {ℓ} {A B : Set ℓ} (Sys : TransformationSystem {ℓ} A B) where
  open Infrastructure.Functor.Interface
  open import Infrastructure.Axiom.Adequacy
  open import Infrastructure.Axiom.Instance

  pathAlgebra : PathAlgebra {ℓV = ℓ} {ℓP = ℓ} (One {ℓ})
  PathAlgebra.Path pathAlgebra _ _ = Path Sys
  PathAlgebra._++_ pathAlgebra = _⊕_
  PathAlgebra.++-assoc pathAlgebra = λ p q r → ⊕-assoc p q r
  PathAlgebra.id pathAlgebra = refl-path
  PathAlgebra.id-left pathAlgebra = ⊕-id-left
  PathAlgebra.id-right pathAlgebra = ⊕-id-right

  -- | Kit holding a single path for adequacy.
  record PathKit : Set ℓ where
    field p : Path Sys

  pathFace : PathKit → FramedFace pathAlgebra
  FramedFace.a    (pathFace k) = tt
  FramedFace.b    (pathFace k) = tt
  FramedFace.face (pathFace k) = record { lhs = PathKit.p k ; rhs = PathKit.p k }

  pathAxiomInstance : AxiomInstance pathAlgebra
  AxiomInstance.Kit   pathAxiomInstance = PathKit
  AxiomInstance.face  pathAxiomInstance = pathFace
  AxiomInstance.solve pathAxiomInstance _ = refl
