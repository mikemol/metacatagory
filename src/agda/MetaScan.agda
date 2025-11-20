{-# OPTIONS --allow-unsolved-metas #-}

module MetaScan where

open import Agda.Builtin.Reflection
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Reflection using (Declaration; Postulate; Name)

-- List all postulates in the current module
scanPostulates : TC (List String)
scanPostulates = do
  decls ← getDeclarations
  pure (List.map declToString (List.filter isPostulate decls))
  where
    isPostulate : Declaration → Bool
    isPostulate (Postulate _ _ _) = true
    isPostulate _ = false
    declToString : Declaration → String
    declToString (Postulate x _ _) = nameToString x
    declToString _ = ""
    nameToString : Name → String
    nameToString x = show x

-- Expose the value for further processing
postulateNames : List String
postulateNames = runTC scanPostulates
