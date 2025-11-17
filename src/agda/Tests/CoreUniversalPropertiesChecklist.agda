-- Tests/CoreUniversalPropertiesChecklist.agda
-- Instantiate core universal property adapters and assert status

module Tests.CoreUniversalPropertiesChecklist where

open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Equality using (_≡_; refl)
import Agda.Builtin.Bool as B
open import Metamodel as M
import Tests.ObligationAdapters as A
import Core.UniversalProperties as CUP

------------------------------------------------------------------------
-- Initial and Terminal Objects
------------------------------------------------------------------------

initialObj : CUP.InitialObject
initialObj = record
  { initial = M.mkId "0"
  ; initialMorphism = λ X → M.mkId "!"
  ; initialUnique = λ X f → M.mkId "uniq"
  }

initial-adapter : A.InitialObjectAdapter
initial-adapter = A.mkInitialObjectAdapter initialObj (CUP.InitialObject.initial initialObj) refl

initial-status : A.isFilledInitialObject initial-adapter ≡ B.true
initial-status = refl

terminalObj : CUP.TerminalObject
terminalObj = record
  { terminal = M.mkId "1"
  ; terminalMorphism = λ X → M.mkId "!"
  ; terminalUnique = λ X f → M.mkId "uniq"
  }

terminal-adapter : A.TerminalObjectAdapter
terminal-adapter = A.mkTerminalObjectAdapter terminalObj (CUP.TerminalObject.terminal terminalObj) refl

terminal-status : A.isFilledTerminalObject terminal-adapter ≡ B.true
terminal-status = refl

------------------------------------------------------------------------
-- Product and Coproduct
------------------------------------------------------------------------

A₀ B₀ : M.Identifier
A₀ = M.mkId "A"
B₀ = M.mkId "B"

prodProp : CUP.ProductProperty A₀ B₀
prodProp = record
  { product = M.mkId "A×B"
  ; π₁ = M.mkId "π₁"
  ; π₂ = M.mkId "π₂"
  ; mediating = λ X f g → M.mkId "⟨f,g⟩"
  ; π₁-commutes = λ X f g → M.mkId "π₁∘⟨f,g⟩=f"
  ; π₂-commutes = λ X f g → M.mkId "π₂∘⟨f,g⟩=g"
  ; mediating-unique = λ X f g h → M.mkId "uniq"
  }

product-adapter : A.ProductPropertyAdapter
product-adapter = A.mkProductPropertyAdapter A₀ B₀ prodProp (CUP.ProductProperty.product prodProp) refl

product-status : A.isFilledProductProperty product-adapter ≡ B.true
product-status = refl

coprodProp : CUP.CoproductProperty A₀ B₀
coprodProp = record
  { coproduct = M.mkId "A+B"
  ; ι₁ = M.mkId "ι₁"
  ; ι₂ = M.mkId "ι₂"
  ; comediating = λ X f g → M.mkId "[f,g]"
  ; ι₁-commutes = λ X f g → M.mkId "[f,g]∘ι₁=f"
  ; ι₂-commutes = λ X f g → M.mkId "[f,g]∘ι₂=g"
  ; comediating-unique = λ X f g h → M.mkId "uniq"
  }

coproduct-adapter : A.CoproductPropertyAdapter
coproduct-adapter = A.mkCoproductPropertyAdapter A₀ B₀ coprodProp (CUP.CoproductProperty.coproduct coprodProp) refl

coproduct-status : A.isFilledCoproductProperty coproduct-adapter ≡ B.true
coproduct-status = refl

------------------------------------------------------------------------
-- Equalizer and Coequalizer
------------------------------------------------------------------------

f g : M.Identifier
f = M.mkId "f"
g = M.mkId "g"

A₁ B₁ : M.Identifier
A₁ = M.mkId "A₁"
B₁ = M.mkId "B₁"

eqProp : CUP.EqualizerProperty A₁ B₁ f g
eqProp = record
  { equalizer = M.mkId "Eq(f,g)"
  ; equalize = M.mkId "e"
  ; equalizes = M.mkId "f∘e=g∘e"
  ; mediating = λ X h → M.mkId "m"
  ; mediating-commutes = λ X h → M.mkId "comm"
  ; mediating-unique = λ X h k → M.mkId "uniq"
  }

equalizer-adapter : A.EqualizerPropertyAdapter
equalizer-adapter = A.mkEqualizerPropertyAdapter A₁ B₁ f g eqProp (CUP.EqualizerProperty.equalizer eqProp) refl

equalizer-status : A.isFilledEqualizerProperty equalizer-adapter ≡ B.true
equalizer-status = refl

coeqProp : CUP.CoequalizerProperty A₁ B₁ f g
coeqProp = record
  { coequalizer = M.mkId "Coeq(f,g)"
  ; coequalize = M.mkId "q"
  ; coequalizes = M.mkId "q∘f=q∘g"
  ; comediating = λ X h → M.mkId "m"
  ; comediating-commutes = λ X h → M.mkId "comm"
  ; comediating-unique = λ X h k → M.mkId "uniq"
  }

coequalizer-adapter : A.CoequalizerPropertyAdapter
coequalizer-adapter = A.mkCoequalizerPropertyAdapter A₁ B₁ f g coeqProp (CUP.CoequalizerProperty.coequalizer coeqProp) refl

coequalizer-status : A.isFilledCoequalizerProperty coequalizer-adapter ≡ B.true
coequalizer-status = refl

------------------------------------------------------------------------
-- Pullback and Pushout
------------------------------------------------------------------------

A₂ B₂ C₂ : M.Identifier
A₂ = M.mkId "A₂"
B₂ = M.mkId "B₂"
C₂ = M.mkId "C₂"

f₂ g₂ : M.Identifier
f₂ = M.mkId "f₂"
g₂ = M.mkId "g₂"

pbProp : CUP.PullbackProperty A₂ B₂ C₂ f₂ g₂
pbProp = record
  { pullback = M.mkId "A₂×_{C₂}B₂"
  ; π₁ = M.mkId "π₁"
  ; π₂ = M.mkId "π₂"
  ; commutes = M.mkId "f∘π₁=g∘π₂"
  ; mediating = λ X h k p → M.mkId "⟨h,k⟩"
  ; π₁-commutes = λ X h k p → M.mkId "π₁∘⟨h,k⟩=h"
  ; π₂-commutes = λ X h k p → M.mkId "π₂∘⟨h,k⟩=k"
  ; mediating-unique = λ X h k p m → M.mkId "uniq"
  }

pullback-adapter : A.PullbackPropertyAdapter
pullback-adapter = A.mkPullbackPropertyAdapter A₂ B₂ C₂ f₂ g₂ pbProp (CUP.PullbackProperty.pullback pbProp) refl

pullback-status : A.isFilledPullbackProperty pullback-adapter ≡ B.true
pullback-status = refl

poProp : CUP.PushoutProperty A₂ B₂ C₂ f₂ g₂
poProp = record
  { pushout = M.mkId "B₂+_{A₂}C₂"
  ; ι₁ = M.mkId "ι₁"
  ; ι₂ = M.mkId "ι₂"
  ; commutes = M.mkId "ι₁∘f=ι₂∘g"
  ; comediating = λ X h k p → M.mkId "[h,k]"
  ; ι₁-commutes = λ X h k p → M.mkId "[h,k]∘ι₁=h"
  ; ι₂-commutes = λ X h k p → M.mkId "[h,k]∘ι₂=k"
  ; comediating-unique = λ X h k p m → M.mkId "uniq"
  }

pushout-adapter : A.PushoutPropertyAdapter
pushout-adapter = A.mkPushoutPropertyAdapter A₂ B₂ C₂ f₂ g₂ poProp (CUP.PushoutProperty.pushout poProp) refl

pushout-status : A.isFilledPushoutProperty pushout-adapter ≡ B.true
pushout-status = refl

------------------------------------------------------------------------
-- Limits and Colimits
------------------------------------------------------------------------

D : M.Identifier
D = M.mkId "D"

limProp : CUP.LimitProperty D
limProp = record
  { limit = M.mkId "lim D"
  ; cone = M.mkId "cone"
  ; mediating = λ X c → M.mkId "m"
  ; mediating-commutes = λ X c → M.mkId "comm"
  ; mediating-unique = λ X c m → M.mkId "uniq"
  }

limit-adapter : A.LimitPropertyAdapter
limit-adapter = A.mkLimitPropertyAdapter D limProp (CUP.LimitProperty.limit limProp) refl

limit-status : A.isFilledLimitProperty limit-adapter ≡ B.true
limit-status = refl

colimProp : CUP.ColimitProperty D
colimProp = record
  { colimit = M.mkId "colim D"
  ; cocone = M.mkId "cocone"
  ; comediating = λ X c → M.mkId "m"
  ; comediating-commutes = λ X c → M.mkId "comm"
  ; comediating-unique = λ X c m → M.mkId "uniq"
  }

colimit-adapter : A.ColimitPropertyAdapter
colimit-adapter = A.mkColimitPropertyAdapter D colimProp (CUP.ColimitProperty.colimit colimProp) refl

colimit-status : A.isFilledColimitProperty colimit-adapter ≡ B.true
colimit-status = refl
