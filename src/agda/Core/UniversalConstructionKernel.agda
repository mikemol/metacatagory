{-# OPTIONS --without-K #-}

-- | Minimal kernel for universal construction signatures and witnesses.
module Core.UniversalConstructionKernel where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Primitive using (Level; lsuc)

open import Metamodel as M

-- ============================================================================
-- Universal Construction Kernel
-- ============================================================================

-- | High-level kind tags for universal constructions.
data ConstructionKind : Set where
  limit : ConstructionKind
  colimit : ConstructionKind
  adjunction : ConstructionKind
  kanExtension : ConstructionKind
  universalProperty : ConstructionKind

-- | Canonical signature describing a universal construction.
record ConstructionSignature : Set₁ where
  field
    constructionId : M.Identifier
    kind : ConstructionKind
    objects : List M.Identifier
    morphisms : List M.Identifier
    propertyId : M.Identifier

-- | Signature paired with a concrete witness for a universal property.
record UniversalConstruction : Set₁ where
  field
    signature : ConstructionSignature
    propertyWitness : M.Identifier

-- ============================================================================
-- Canonical Signatures (Minimal Set)
-- ============================================================================

productSignature : (A B : M.Identifier) → ConstructionSignature
productSignature A B = record
  { constructionId = M.mkId "Product"
  ; kind = limit
  ; objects = A ∷ B ∷ []
  ; morphisms = []
  ; propertyId = M.mkId "ProductProperty"
  }

coproductSignature : (A B : M.Identifier) → ConstructionSignature
coproductSignature A B = record
  { constructionId = M.mkId "Coproduct"
  ; kind = colimit
  ; objects = A ∷ B ∷ []
  ; morphisms = []
  ; propertyId = M.mkId "CoproductProperty"
  }

equalizerSignature : (A B : M.Identifier) → ConstructionSignature
equalizerSignature A B = record
  { constructionId = M.mkId "Equalizer"
  ; kind = limit
  ; objects = A ∷ B ∷ []
  ; morphisms = []
  ; propertyId = M.mkId "EqualizerProperty"
  }

coequalizerSignature : (A B : M.Identifier) → ConstructionSignature
coequalizerSignature A B = record
  { constructionId = M.mkId "Coequalizer"
  ; kind = colimit
  ; objects = A ∷ B ∷ []
  ; morphisms = []
  ; propertyId = M.mkId "CoequalizerProperty"
  }

pullbackSignature : (A B C : M.Identifier) → ConstructionSignature
pullbackSignature A B C = record
  { constructionId = M.mkId "Pullback"
  ; kind = limit
  ; objects = A ∷ B ∷ C ∷ []
  ; morphisms = []
  ; propertyId = M.mkId "PullbackProperty"
  }

pushoutSignature : (A B C : M.Identifier) → ConstructionSignature
pushoutSignature A B C = record
  { constructionId = M.mkId "Pushout"
  ; kind = colimit
  ; objects = A ∷ B ∷ C ∷ []
  ; morphisms = []
  ; propertyId = M.mkId "PushoutProperty"
  }

adjunctionSignature : (C D : M.Identifier) → ConstructionSignature
adjunctionSignature C D = record
  { constructionId = M.mkId "Adjunction"
  ; kind = adjunction
  ; objects = C ∷ D ∷ []
  ; morphisms = []
  ; propertyId = M.mkId "AdjunctionProperty"
  }

kanExtensionSignature : (C D : M.Identifier) → ConstructionSignature
kanExtensionSignature C D = record
  { constructionId = M.mkId "KanExtension"
  ; kind = kanExtension
  ; objects = C ∷ D ∷ []
  ; morphisms = []
  ; propertyId = M.mkId "KanExtensionProperty"
  }
