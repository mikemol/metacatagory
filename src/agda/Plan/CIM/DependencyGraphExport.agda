{-# OPTIONS --without-K #-}

-- | DependencyGraphExport: consume Agda --dependency-graph DOT output and emit JSON.
-- This replaces the Python dependency_graph.json generator with a pure Agda version.
module Plan.CIM.DependencyGraphExport where

open import Agda.Builtin.String using (String; primStringAppend; primStringFromList; primStringToList; primStringEquality)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Infrastructure.Equality using (trans)  -- simple helper
open import Core.Strings using (natToString)

-- Basic string helpers
_++_ : String → String → String
_++_ = primStringAppend

infixr 20 _++_

_++ˡ_ : ∀ {A : Set} → List A → List A → List A
[] ++ˡ ys = ys
(x ∷ xs) ++ˡ ys = x ∷ (xs ++ˡ ys)

-- Character comparison
_==c_ : Char → Char → Bool
'a' ==c 'a' = true
'b' ==c 'b' = true
'c' ==c 'c' = true
'd' ==c 'd' = true
'e' ==c 'e' = true
'f' ==c 'f' = true
'g' ==c 'g' = true
'h' ==c 'h' = true
'i' ==c 'i' = true
'j' ==c 'j' = true
'k' ==c 'k' = true
'l' ==c 'l' = true
'm' ==c 'm' = true
'n' ==c 'n' = true
'o' ==c 'o' = true
'p' ==c 'p' = true
'q' ==c 'q' = true
'r' ==c 'r' = true
's' ==c 's' = true
't' ==c 't' = true
'u' ==c 'u' = true
'v' ==c 'v' = true
'w' ==c 'w' = true
'x' ==c 'x' = true
'y' ==c 'y' = true
'z' ==c 'z' = true
'0' ==c '0' = true
'1' ==c '1' = true
'2' ==c '2' = true
'3' ==c '3' = true
'4' ==c '4' = true
'5' ==c '5' = true
'6' ==c '6' = true
'7' ==c '7' = true
'8' ==c '8' = true
'9' ==c '9' = true
'[' ==c '[' = true
']' ==c ']' = true
'{' ==c '{' = true
'}' ==c '}' = true
'(' ==c '(' = true
')' ==c ')' = true
';' ==c ';' = true
'=' ==c '=' = true
'"' ==c '"' = true
'-' ==c '-' = true
'>' ==c '>' = true
'_' ==c '_' = true
'.' ==c '.' = true
' ' ==c ' ' = true
'\t' ==c '\t' = true
'\n' ==c '\n' = true
_ ==c _ = false

infixr 15 _==c_

-- Simple file IO (FFI)
{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
#-}

postulate
  readFile : String → IO String
  writeFile : String → String → IO ⊤
  _>>=_ : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} → IO A → (A → IO B) → IO B
  return : ∀ {ℓ} {A : Set ℓ} → A → IO A
{-# COMPILE GHC readFile = \path -> do { txt <- TIO.readFile (T.unpack path); return (T.pack (T.unpack txt)) } #-}
{-# COMPILE GHC writeFile = \path content -> TIO.writeFile (T.unpack path) content #-}
{-# COMPILE GHC _>>=_ = \_ _ _ _ m f -> m >>= f #-}
{-# COMPILE GHC return = \_ _ x -> return x #-}

_>>_ : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} → IO A → IO B → IO B
_>>_ m n = _>>=_ m (λ _ → n)

------------------------------------------------------------------------
-- Minimal JSON rendering
------------------------------------------------------------------------

escapeChar : Char → List Char
escapeChar c with c
... | '"'  = '\\' ∷ '"'  ∷ []
... | '\\' = '\\' ∷ '\\' ∷ []
... | '\n' = '\\' ∷ 'n'  ∷ []
... | '\r' = '\\' ∷ 'r'  ∷ []
... | '\t' = '\\' ∷ 't'  ∷ []
... | _    = c ∷ []

escapeChars : List Char → List Char
escapeChars [] = []
escapeChars (c ∷ cs) = escapeChar c ++ˡ escapeChars cs

escapeString : String → String
escapeString s = primStringFromList (escapeChars (primStringToList s))

quoteString : String → String
quoteString s = "\"" ++ escapeString s ++ "\""

renderStringList : List String → String
renderStringList [] = "[]"
renderStringList (x ∷ xs) = "[" ++ quoteString x ++ renderRest xs ++ "]"
  where
    renderRest : List String → String
    renderRest [] = ""
    renderRest (y ∷ ys) = "," ++ quoteString y ++ renderRest ys

concatStrings : List String → String
concatStrings [] = ""
concatStrings (x ∷ xs) = x ++ concatStrings xs

reverse : ∀ {A : Set} → List A → List A
reverse [] = []
reverse (x ∷ xs) = reverse xs ++ˡ (x ∷ [])

map : ∀ {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

add : Nat → Nat → Nat
add m n = _+_ m n

-- | Simple pair used in parsing/encoding edges.
record _×_ (A B : Set) : Set where
  constructor _,_
  field fst : A
        snd : B

open _×_ public

------------------------------------------------------------------------
-- Parse DOT node IDs and labels: m0[label="Module.Name"];
------------------------------------------------------------------------

-- | Parse a single line to extract node ID and label
-- Format: m123[label="Module.Qualified.Name"];
record DotNode : Set where
  constructor mkDotNode
  field nodeId label : String

-- Simple DOT line parsing (labels)
mutual
  parseDotLabel : String → Maybe DotNode
  parseDotLabel s = parseDotLabelHelper (primStringToList s) [] false false

  parseDotLabelHelper : List Char → List Char → Bool → Bool → Maybe DotNode
  parseDotLabelHelper [] _ _ _ = nothing
  -- Found [label=" sequence
  parseDotLabelHelper ('l' ∷ 'a' ∷ 'b' ∷ 'e' ∷ 'l' ∷ '=' ∷ '"' ∷ rest) nodeAcc false _ =
    parseLabelContent rest nodeAcc []
  -- Found [ but not label yet
  parseDotLabelHelper ('[' ∷ rest) nodeAcc false _ = 
    parseDotLabelHelper rest nodeAcc false true
  -- Accumulating node ID
  parseDotLabelHelper (c ∷ rest) nodeAcc false false with c ==c ' ' | c ==c '\t'
  ... | true  | _ = parseDotLabelHelper rest nodeAcc false false
  ... | _     | true = parseDotLabelHelper rest nodeAcc false false
  ... | false | false = parseDotLabelHelper rest (c ∷ nodeAcc) false false
  -- Inside label attribute but haven't found "label=" yet
  parseDotLabelHelper (c ∷ rest) nodeAcc false true =
    parseDotLabelHelper rest nodeAcc false true
  parseDotLabelHelper _ _ true _ = nothing

  parseLabelContent : List Char → List Char → List Char → Maybe DotNode
  parseLabelContent [] _ _ = nothing
  parseLabelContent ('"' ∷ rest) nodeAcc labelAcc =
    just (mkDotNode (primStringFromList (reverse nodeAcc)) (primStringFromList (reverse labelAcc)))
  parseLabelContent (c ∷ rest) nodeAcc labelAcc =
    parseLabelContent rest nodeAcc (c ∷ labelAcc)

------------------------------------------------------------------------
-- Parse DOT edge lines: m0 -> m43;
------------------------------------------------------------------------

-- | Directed edge (src → dst) extracted from DOT.
record Edge : Set where
  constructor mkEdge
  field src dst : String

-- Simple DOT line parsing (edges)
mutual
  parseEdgeIds : String → Maybe Edge
  parseEdgeIds s = parseEdgeHelper (primStringToList s) [] false

  parseEdgeHelper : List Char → List Char → Bool → Maybe Edge
  parseEdgeHelper [] _ _ = nothing
  parseEdgeHelper (c ∷ cs) acc false with c ==c ' ' | c ==c '\t'
  ... | true  | _ = parseEdgeHelper cs acc false
  ... | _     | true = parseEdgeHelper cs acc false
  ... | false | false with c ==c '-'
  ...   | true = parseArrowHelper cs acc []
  ...   | false = parseEdgeHelper cs (c ∷ acc) false
  parseEdgeHelper _ _ true = nothing

  parseArrowHelper : List Char → List Char → List Char → Maybe Edge
  parseArrowHelper [] _ _ = nothing
  parseArrowHelper ('>' ∷ cs) srcAcc dstAcc = 
    parseDestHelper cs srcAcc dstAcc []
  parseArrowHelper (c ∷ cs) srcAcc dstAcc with c ==c ' ' | c ==c '\t'
  ... | true  | _ = parseArrowHelper cs srcAcc dstAcc
  ... | _     | true = parseArrowHelper cs srcAcc dstAcc
  ... | false | false = parseDestHelper cs srcAcc [] (c ∷ [])

  parseDestHelper : List Char → List Char → List Char → List Char → Maybe Edge
  parseDestHelper [] _ _ _ = nothing
  parseDestHelper (';' ∷ cs) srcAcc unused dstAcc = 
    just (mkEdge (primStringFromList (reverse srcAcc)) (primStringFromList (reverse dstAcc)))
  parseDestHelper (c ∷ cs) srcAcc unused dstAcc with c ==c ' ' | c ==c '\t'
  ... | true  | _ = parseDestHelper cs srcAcc unused dstAcc
  ... | _     | true = parseDestHelper cs srcAcc unused dstAcc
  ... | false | false = parseDestHelper cs srcAcc unused (c ∷ dstAcc)

------------------------------------------------------------------------
-- Build adjacency from edges
------------------------------------------------------------------------

record NodeDeps : Set where
  constructor mkNodeDeps
  field imports importedBy : List String

lookupNode : String → List (String × NodeDeps) → Maybe NodeDeps
lookupNode k [] = nothing
lookupNode k ((k' , v) ∷ rest) with primStringEquality k k'
... | true  = just v
... | false = lookupNode k rest

insertEdge : Edge → List (String × NodeDeps) → List (String × NodeDeps)
insertEdge e [] = (Edge.src e , mkNodeDeps (Edge.dst e ∷ []) []) ∷
                  (Edge.dst e , mkNodeDeps [] []) ∷ []
insertEdge e ((k , v) ∷ rest) with primStringEquality k (Edge.src e) | primStringEquality k (Edge.dst e)
... | true  | _    = (k , mkNodeDeps (Edge.dst e ∷ NodeDeps.imports v) (NodeDeps.importedBy v)) ∷ rest
... | _     | true = (k , mkNodeDeps (NodeDeps.imports v) (Edge.src e ∷ NodeDeps.importedBy v)) ∷ rest
... | _     | _    = (k , v) ∷ insertEdge e rest

foldEdges : List Edge → List (String × NodeDeps) → List (String × NodeDeps)
foldEdges [] acc = acc
foldEdges (e ∷ es) acc = foldEdges es (insertEdge e acc)

------------------------------------------------------------------------
-- Render JSON nodes
------------------------------------------------------------------------

renderNode : String × NodeDeps → String
renderNode (k , v) =
  concatStrings
    ( "{" ∷
      "\"module\":"      ∷ quoteString k ∷ "," ∷
      "\"imports\":"     ∷ renderStringList (NodeDeps.imports v) ∷ "," ∷
      "\"imported_by\":" ∷ renderStringList (NodeDeps.importedBy v) ∷
      "}" ∷ [] )

renderNodes : List (String × NodeDeps) → String
renderNodes [] = "[]"
renderNodes (x ∷ xs) = "[" ++ renderNode x ++ renderRest xs ++ "]"
  where
    renderRest : List (String × NodeDeps) → String
    renderRest [] = ""
    renderRest (y ∷ ys) = "," ++ renderNode y ++ renderRest ys

------------------------------------------------------------------------
-- Top-level export
------------------------------------------------------------------------

defaultInPath : String
defaultInPath = "build/diagrams/agda-deps-full.dot"

defaultOutPath : String
defaultOutPath = "build/dependency_graph.json"

count : ∀ {A : Set} → List A → Nat
count [] = zero
count (_ ∷ xs) = suc (count xs)

renderJson : List (String × NodeDeps) → String
renderJson nodes =
  "{" ++
    "\"metadata\":{" ++
      "\"total_modules\":" ++ natToString (count nodes) ++ "," ++
      "\"total_dependencies\":" ++ natToString (edgeCount nodes) ++ "," ++
      "\"cycles_detected\":0," ++
      "\"critical_path_length\":0," ++
      "\"dependency_layers\":0," ++
      "\"timestamp\":\"\"}," ++
    "\"nodes\":" ++ renderNodes nodes ++
  "}"
  where
    edgeCount : List (String × NodeDeps) → Nat
    edgeCount [] = zero
    edgeCount ((k , v) ∷ xs) = add (count (NodeDeps.imports v)) (edgeCount xs)

------------------------------------------------------------------------
-- Line-based parsing
------------------------------------------------------------------------

-- | Parse all lines to extract node labels and edges separately
parseLabelLines : List Char → List DotNode
parseLabelLines cs = go [] cs
  where
    mutual
      go : List Char → List Char → List DotNode
      go acc [] = maybeCons acc []
      go acc ('\n' ∷ rest) = maybeCons acc (go [] rest)
      go acc (c ∷ rest) = go (c ∷ acc) rest

      maybeCons : List Char → List DotNode → List DotNode
      maybeCons [] ns = ns
      maybeCons cs ns with parseDotLabel (primStringFromList (reverse cs))
      ... | nothing = ns
      ... | just n  = n ∷ ns

parseEdgeLines : List Char → List Edge
parseEdgeLines cs = go [] cs
  where
    mutual
      go : List Char → List Char → List Edge
      go acc [] = maybeCons acc []
      go acc ('\n' ∷ rest) = maybeCons acc (go [] rest)
      go acc (c ∷ rest) = go (c ∷ acc) rest

      maybeCons : List Char → List Edge → List Edge
      maybeCons [] es = es
      maybeCons cs es with parseEdgeIds (primStringFromList (reverse cs))
      ... | nothing = es
      ... | just e  = e ∷ es

-- | Build a map from node ID (m0, m1, etc.) to module name
buildLabelMap : List DotNode → List (String × String)
buildLabelMap [] = []
buildLabelMap (n ∷ ns) = (DotNode.nodeId n , DotNode.label n) ∷ buildLabelMap ns

-- | Look up a node ID in the label map
lookupNodeLabel : String → List (String × String) → String
lookupNodeLabel k [] = k  -- fallback to ID if not found
lookupNodeLabel k ((k' , label) ∷ rest) with primStringEquality k k'
... | true  = label
... | false = lookupNodeLabel k rest

main : IO ⊤
main = do
  dot ← readFile defaultInPath
  let cs = primStringToList dot
  let dotNodes = parseLabelLines cs
  let labelMap = buildLabelMap dotNodes
  let edgeIds = parseEdgeLines cs
  -- Resolve edge node IDs to module names
  let edges = map (λ e → mkEdge (lookupNodeLabel (Edge.src e) labelMap) (lookupNodeLabel (Edge.dst e) labelMap)) edgeIds
  let nodes = foldEdges edges []
  writeFile defaultOutPath (renderJson nodes)
