{-# OPTIONS --without-K #-}

-- | Rendering helpers for CIM/Core types (strings, blocks, inline content).
module Core.Rendering where

open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.Bool using (Bool; true; false)

-- | Common rendering utilities for structured data
-- Provides type-safe rendering of structured data to various formats

open import Infrastructure.Universe using (Setℓ)
open import Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
open import Core.Strings using (_++_; intercalate; natToString; quoteJSON; mapWithPrefix)

-- ==========================================================
-- Markdown Rendering
-- ==========================================================

record MarkdownSection : Set where
  -- | A single markdown heading with paragraph lines.
  field
    heading : String
    content : List String

record MarkdownDocument : Set where
  -- | Entire markdown document with metadata and ordered sections.
  field
    title : String
    metadata : List String  -- e.g., timestamp, author
    sections : List MarkdownSection

-- Render markdown section
-- | Pretty-print a markdown section with heading and paragraphs.
renderMarkdownSection : MarkdownSection → String
renderMarkdownSection section =
  "## " ++ (MarkdownSection.heading section ++ ("\n\n" ++ intercalate "\n" (MarkdownSection.content section)))

-- Render markdown document
-- | Render an entire markdown document with metadata and sections.
renderMarkdownDocument : MarkdownDocument → String
renderMarkdownDocument doc =
  let header = "# " ++ (MarkdownDocument.title doc ++ "\n")
      metaList = MarkdownDocument.metadata doc
      meta = renderMeta metaList
      sectionStrs = map renderMarkdownSection (MarkdownDocument.sections doc)
  in header ++ (meta ++ intercalate "\n\n" sectionStrs)
  where
    null : {A : Set} → List A → Bool
    null [] = true
    null (_ ∷ _) = false
    
    renderMeta : List String → String
    renderMeta [] = ""
    renderMeta xs = intercalate "\n" xs ++ "\n\n"
    
    map : {A B : Set} → (A → B) → List A → List B
    map f [] = []
    map f (x ∷ xs) = f x ∷ map f xs
    
    open import Agda.Builtin.Bool using (Bool; true; false)

-- ==========================================================
-- JSON Rendering
-- ==========================================================

record JSONField : Set where
  field
    key : String
    value : String  -- Pre-formatted value (number, string, etc.)

record JSONObject : Set where
  field
    fields : List JSONField

-- Render JSON field (key: value pair)
renderJSONField : JSONField → String
renderJSONField fld =
  "  " ++ (quoteJSON (JSONField.key fld) ++ (": " ++ JSONField.value fld))

-- Helper to reverse a list
reverse : {A : Set} → List A → List A
reverse = reverseAcc []
  where
    reverseAcc : {A : Set} → List A → List A → List A
    reverseAcc acc [] = acc
    reverseAcc acc (x ∷ xs) = reverseAcc (x ∷ acc) xs

-- Helper: list concatenation
infixr 5 _+++_
_+++_ : {A : Set} → List A → List A → List A
[] +++ ys = ys
(x ∷ xs) +++ ys = x ∷ (xs +++ ys)

-- Helper: map
map : {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

-- Render JSON object
renderJSONObject : JSONObject → String
renderJSONObject obj =
  renderFields (reverse (map renderJSONField (JSONObject.fields obj)))
  where
    renderFields : List String → String
    renderFields [] = "{\n}\n"
    renderFields (lastField ∷ rest) =
      let allButLast = reverse rest
          fieldsWithCommas = map (_++ ",") allButLast +++ (lastField ∷ [])
      in "{\n" ++ (intercalate "\n" fieldsWithCommas ++ "\n}\n")

-- Create JSON field for string value
jsonStringField : String → String → JSONField
jsonStringField key val = record { key = key ; value = quoteJSON val }

-- Create JSON field for number value  
jsonNumberField : String → Nat → JSONField
jsonNumberField key val = record { key = key ; value = natToString val }

-- ==========================================================
-- Table Rendering
-- ==========================================================

-- | Simple row of cell strings.
record TableRow : Set where
  field
    cells : List String

-- | Markdown-like table with headers and body rows.
record Table : Set where
  field
    headers : List String
    rows : List TableRow

-- Render markdown table
renderMarkdownTable : Table → List String
renderMarkdownTable table =
  let headerRow = "| " ++ (intercalate " | " (Table.headers table) ++ " |")
      separatorRow = "|" ++ (intercalate "|" (replicate (length (Table.headers table)) "----------") ++ "|")
      dataRows = map renderRow (Table.rows table)
  in headerRow ∷ separatorRow ∷ dataRows
  where
    renderRow : TableRow → String
    renderRow row = "| " ++ (intercalate " | " (TableRow.cells row) ++ " |")
    
    length : {A : Set} → List A → Nat
    length [] = 0
    length (_ ∷ xs) = suc (length xs)
    
    replicate : {A : Set} → Nat → A → List A
    replicate zero x = []
    replicate (suc n) x = x ∷ replicate n x
