{-# OPTIONS --without-K --cubical-compatible #-}
open import Agda.Builtin.List using (List; _∷_; [])

-- | Show/pretty-print inline Pandoc AST nodes.
module Plan.CIM.PandocShowInline where

open import Agda.Builtin.String
open import Plan.CIM.PandocAST using (Inline; MdInline)
open Plan.CIM.PandocAST

infixr 20 _++_
_++_ : String → String → String
_++_ = primStringAppend

showInlines : List Inline → String
showInlines [] = ""





showInlines (Str s ∷ xs) = _++_ (_++_ "Str '" s) (_++_ "' " (showInlines xs))
showInlines (Emph ys ∷ xs) = _++_ (_++_ "Emph [" (showInlines ys)) (_++_ "] " (showInlines xs))
showInlines (Strong ys ∷ xs) = _++_ (_++_ "Strong [" (showInlines ys)) (_++_ "] " (showInlines xs))
showInlines (Code s ∷ xs) = _++_ (_++_ "Code '" s) (_++_ "' " (showInlines xs))
showInlines (Space ∷ xs) = _++_ "Space " (showInlines xs)
showInlines (SoftBreak ∷ xs) = _++_ "SoftBreak " (showInlines xs)
showInlines (LineBreak ∷ xs) = _++_ "LineBreak " (showInlines xs)
showInlines (Math s ∷ xs) = _++_ (_++_ "Math '" s) (_++_ "' " (showInlines xs))
showInlines (RawInline s ∷ xs) = _++_ (_++_ "RawInline '" s) (_++_ "' " (showInlines xs))
showInlines (Link ys url ∷ xs) = _++_ (_++_ (_++_ "Link [" (showInlines ys)) (_++_ "] '" url)) (_++_ "' " (showInlines xs))
showInlines (Image ys u ∷ xs) = _++_ (_++_ (_++_ "Image [" (showInlines ys)) (_++_ "] '" u)) (_++_ "' " (showInlines xs))
showInlines (Note ys ∷ xs) = _++_ (_++_ "Note [" (showInlines ys)) (_++_ "] " (showInlines xs))

showMdInlines : List MdInline → String
showMdInlines [] = ""
showMdInlines (MdStr s ∷ xs) = _++_ (_++_ "MdStr '" s) (_++_ "' " (showMdInlines xs))
showMdInlines (MdEmph ys ∷ xs) = _++_ (_++_ "MdEmph [" (showMdInlines ys)) (_++_ "] " (showMdInlines xs))
showMdInlines (MdStrong ys ∷ xs) = _++_ (_++_ "MdStrong [" (showMdInlines ys)) (_++_ "] " (showMdInlines xs))
showMdInlines (MdCode s ∷ xs) = _++_ (_++_ "MdCode '" s) (_++_ "' " (showMdInlines xs))
showMdInlines (MdSpace ∷ xs) = _++_ "MdSpace " (showMdInlines xs)
showMdInlines (MdBreak ∷ xs) = _++_ "MdBreak " (showMdInlines xs)
showMdInlines (MdEOL ∷ xs) = _++_ "MdEOL " (showMdInlines xs)
showMdInlines (MdLink ys url ∷ xs) = _++_ (_++_ (_++_ "MdLink [" (showMdInlines ys)) (_++_ "] '" url)) (_++_ "' " (showMdInlines xs))
showMdInlines (MdImage ys u ∷ xs) = _++_ (_++_ (_++_ "MdImage [" (showMdInlines ys)) (_++_ "] '" u)) (_++_ "' " (showMdInlines xs))
