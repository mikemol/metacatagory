{-# OPTIONS --without-K --cubical-compatible #-}

module Plan.CIM.RoadmapExporter where

open import Agda.Builtin.String using (String; primStringAppend; primStringEquality)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)

open import Plan.CIM.Utility hiding (_++_; map)
open import Plan.CIM.PandocAST
open import Plan.CIM.DocumentationContent
open import Plan.CIM.DocumentSynthesis

map : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

_++ˡ_ : ∀ {ℓ} {A : Set ℓ} → List A → List A → List A
[] ++ˡ ys = ys
(x ∷ xs) ++ˡ ys = x ∷ (xs ++ˡ ys)


{-# TERMINATING #-}
transformInline : Inline → MdInline
transformInline (Str s)       = MdStr s
transformInline (Emph xs)     = MdEmph (map transformInline xs)
transformInline (Strong xs)   = MdStrong (map transformInline xs)
transformInline (Code s)      = MdCode s
transformInline Space         = MdSpace
transformInline SoftBreak     = MdBreak
transformInline LineBreak     = MdBreak
transformInline (Math s)      = MdCode s
transformInline (RawInline s) = MdStr s
transformInline (Link xs url) = MdLink (map transformInline xs) url
transformInline (Image xs u)  = MdImage (map transformInline xs) u
transformInline (Note xs)     = MdEmph (map transformInline xs)

{-# TERMINATING #-}
transformBlock : Block → MdBlock
transformBlock (Para xs)        = MdPara (map transformInline xs)
transformBlock (Plain xs)       = MdPara (map transformInline xs)
transformBlock (Header n xs)    = MdHeader n (map transformInline xs)
transformBlock (CodeBlock s)    = MdCodeBlock s
transformBlock (RawBlock s)     = MdRaw s
transformBlock (BlockQuote bs)  = MdQuote (map transformBlock bs)
transformBlock (OrderedList xs) = MdList (map (map transformBlock) xs)
transformBlock (BulletList xs)  = MdList (map (map transformBlock) xs)
transformBlock HorizontalRule   = MdRule
transformBlock (Table s)        = MdRaw s
transformBlock Null             = MdNull

pandocDocToMarkdown : PandocDoc → MarkdownDoc
pandocDocToMarkdown doc = record
  { blocks = map transformBlock (PandocDoc.blocks doc)
  ; meta   = PandocDoc.meta doc
  }

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Adapters to convert Agda String (Text) to Haskell FilePath and back
writeFileAdapter :: T.Text -> T.Text -> IO ()
writeFileAdapter path content = TIO.writeFile (T.unpack path) content

putStrLnAdapter :: T.Text -> IO ()
putStrLnAdapter = TIO.putStrLn
#-}

------------------------------------------------------------------------
-- String utilities
------------------------------------------------------------------------

infixr 20 _++_
_++_ : String → String → String
_++_ = primStringAppend

eqString : String → String → Bool
eqString = primStringEquality

memberString : String → List String → Bool
memberString _ [] = false
memberString s (x ∷ xs) with eqString s x
... | true = true
... | false = memberString s xs

infixl 1 _>>=M_
_>>=M_ : ∀ {ℓ} {A B : Set ℓ} → Maybe A → (A → Maybe B) → Maybe B
just x >>=M f = f x
nothing >>=M _ = nothing

------------------------------------------------------------------------
-- Markdown rendering from MarkdownDoc (structured → string)
------------------------------------------------------------------------

renderInlines : List MdInline → String
renderInlines [] = ""
renderInlines (x ∷ xs) = renderInline x ++ renderInlines xs
  where
    renderInline : MdInline → String
    renderInline (MdStr s) = s
    renderInline (MdEmph ys) = "_" ++ renderInlines ys ++ "_"
    renderInline (MdStrong ys) = "**" ++ renderInlines ys ++ "**"
    renderInline (MdCode s) = "`" ++ s ++ "`"
    renderInline MdSpace = " "
    renderInline MdBreak = "\n"
    renderInline (MdLink ys url) = "[" ++ renderInlines ys ++ "](" ++ url ++ ")"
    renderInline (MdImage ys url) = "![" ++ renderInlines ys ++ "](" ++ url ++ ")"

repeatHash : Nat → String
repeatHash zero = ""
repeatHash (suc n) = "#" ++ repeatHash n

mutual
  renderBlocks : List MdBlock → String
  renderBlocks [] = ""
  renderBlocks (b ∷ bs) = renderBlock b ++ delim bs ++ renderBlocks bs

  delim : List MdBlock → String
  delim [] = ""
  delim (_ ∷ _) = "\n\n"

  renderBlock : MdBlock → String
  renderBlock (MdPara ys) = renderInlines ys
  renderBlock (MdHeader n ys) = repeatHash n ++ " " ++ renderInlines ys
  renderBlock (MdCodeBlock s) = "```text\n" ++ s ++ "\n```"
  renderBlock (MdList items) = renderList items
  renderBlock (MdQuote qs) = renderQuote qs
  renderBlock MdRule = "---"
  renderBlock (MdRaw s) = s
  renderBlock MdNull = ""

  renderList : List (List MdBlock) → String
  renderList [] = ""
  renderList (item ∷ rest) = "* " ++ renderItem item ++ listTail rest

  renderItem : List MdBlock → String
  renderItem [] = ""
  renderItem (ib ∷ ibs) = renderBlock ib ++ itemTail ibs

  itemTail : List MdBlock → String
  itemTail [] = ""
  itemTail (ib ∷ ibs) = "\n  " ++ renderBlock ib ++ itemTail ibs

  listTail : List (List MdBlock) → String
  listTail [] = ""
  listTail (item' ∷ rest') = "\n" ++ renderList (item' ∷ rest')

  renderQuote : List MdBlock → String
  renderQuote qs = "> " ++ renderBlocks qs

renderDoc : MarkdownDoc → String
renderDoc doc = renderBlocks (MarkdownDoc.blocks doc) ++ "\n"

describeRoadmap : RoadmapStep → MdBlock
describeRoadmap s = MdPara
  ( MdStrong (MdStr (RoadmapStep.step s) ∷ [])
  ∷ MdStr " — "
  ∷ MdStr (RoadmapStep.implication s)
  ∷ MdStr " [status: "
  ∷ MdStr (RoadmapStep.status s)
  ∷ MdStr "]"
  ∷ MdBreak
  ∷ MdStr "Target: "
  ∷ MdCode (RoadmapStep.targetModule s)
  ∷ []
  )

record RoadmapNode : Set₁ where
  field
    nodeId : String
    deps   : List String
    step   : RoadmapStep

findNode : String → List RoadmapNode → Maybe RoadmapNode
findNode _ [] = nothing
findNode k (n ∷ ns) with eqString k (RoadmapNode.nodeId n)
... | true  = just n
... | false = findNode k ns

memberNodeId : String → List RoadmapNode → Bool
memberNodeId _ [] = false
memberNodeId s (n ∷ ns) with eqString s (RoadmapNode.nodeId n)
... | true  = true
... | false = memberNodeId s ns

TopoResult : Set₁
TopoResult = Maybe (List RoadmapNode)

{-# TERMINATING #-}
sortTopo : List RoadmapNode → TopoResult
sortTopo nodes = dfsAll nodes []
  where
    mutual
      dfsAll : List RoadmapNode → List RoadmapNode → TopoResult
      dfsAll [] order = just order
      dfsAll (n ∷ ns) order with memberNodeId (RoadmapNode.nodeId n) order
      ... | true  = dfsAll ns order
      ... | false = dfs n [] order >>=M λ ord' → dfsAll ns ord'

      dfs : RoadmapNode → List String → List RoadmapNode → TopoResult
      dfs n temp order with memberNodeId (RoadmapNode.nodeId n) order
      ... | true  = just order
      ... | false with memberString (RoadmapNode.nodeId n) temp
      ...   | true  = nothing -- cycle detected
      ...   | false = dfsDeps (RoadmapNode.deps n) (RoadmapNode.nodeId n ∷ temp) order >>=M λ ord' →
                      just (ord' ++ˡ (n ∷ []))

      dfsDeps : List String → List String → List RoadmapNode → TopoResult
      dfsDeps [] _ order = just order
      dfsDeps (d ∷ ds) temp order =
        findNode d nodes >>=M λ nd →
        dfs nd temp order >>=M λ ord' →
        dfsDeps ds temp ord'

mkNode : String → RoadmapStep → RoadmapNode
mkNode id s = record { nodeId = id ; deps = RoadmapStep.relatedNodes s ; step = s }

roadmapGraph : List RoadmapNode
roadmapGraph =
  mkNode "exampleAlgebraicAmbiguityRoadmap" exampleAlgebraicAmbiguityRoadmap ∷
  mkNode "exampleMetricizationRoadmap" exampleMetricizationRoadmap ∷
  mkNode "exampleTransformationSystemRoadmap" exampleTransformationSystemRoadmap ∷
  mkNode "exampleFunctorialConstructsRoadmap" exampleFunctorialConstructsRoadmap ∷
  mkNode "exampleElasticityOfMeaningRoadmap" exampleElasticityOfMeaningRoadmap ∷
  mkNode "exampleDimensionalReliefRoadmap" exampleDimensionalReliefRoadmap ∷
  mkNode "examplePolytopeManifestRoadmap" examplePolytopeManifestRoadmap ∷
  mkNode "exampleUnifiedTopologicalParserRoadmap" exampleUnifiedTopologicalParserRoadmap ∷
  []

lookupTitle : String → String
lookupTitle s with findNode s roadmapGraph
... | just nd = RoadmapStep.step (RoadmapNode.step nd)
... | nothing = s

lookupModule : String → String
lookupModule s with findNode s roadmapGraph
... | just nd = RoadmapStep.targetModule (RoadmapNode.step nd)
... | nothing = s

depDisplay : String → String
depDisplay s = lookupTitle s ++ " — " ++ lookupModule s

roadmapNodeItem : RoadmapNode → List MdBlock
roadmapNodeItem n with RoadmapNode.deps n
... | [] = describeRoadmap (RoadmapNode.step n) ∷ []
... | ds = describeRoadmap (RoadmapNode.step n) ∷
          MdPara (MdStr "Depends on: " ∷ MdCode (concatWithSep ", " (map depDisplay ds)) ∷ []) ∷
          []

roadmapGraphBlock : List RoadmapNode → MdBlock
roadmapGraphBlock graph with sortTopo graph
... | nothing = MdPara (MdStr "Cycle detected in roadmap dependencies. Please fix the graph." ∷ [])
... | just ordered = MdList (map roadmapNodeItem ordered)

{-# TERMINATING #-}
roadmapsToMdBlocks : List RoadmapStep → MdBlock
roadmapsToMdBlocks steps = MdList (map roadmapItem steps)
  where
    roadmapItem : RoadmapStep → List MdBlock
    roadmapItem s with RoadmapStep.next s
    ... | [] = describeRoadmap s ∷ []
    ... | children = describeRoadmap s ∷ roadmapsToMdBlocks children ∷ []
readmeMarkdown : String
readmeMarkdown =
  let mdoc = pandocDocToMarkdown readmeDoc
      extra = MdHeader 2 (MdStr "Development Roadmaps" ∷ []) ∷ roadmapGraphBlock roadmapGraph ∷
              MdHeader 2 (MdStr "Building" ∷ []) ∷
              MdCodeBlock "make agda-all  # Compile all Agda modules\nmake docs            # Generate documentation" ∷
              []
  in renderDoc (record { blocks = MarkdownDoc.blocks mdoc ++ˡ extra ; meta = MarkdownDoc.meta mdoc })

contributingMarkdown : String
contributingMarkdown =
  renderDoc (pandocDocToMarkdown contributingDoc)

navigationMarkdown : String
navigationMarkdown =
  renderDoc (pandocDocToMarkdown navigationDoc)

roadmapMarkdown : String
roadmapMarkdown =
  let mdoc = pandocDocToMarkdown roadmapDoc
      extra = MdHeader 2 (MdStr "Structured Roadmaps" ∷ []) ∷ roadmapGraphBlock roadmapGraph ∷
              MdHeader 2 (MdStr "Deferred Snapshot" ∷ []) ∷
              MdList (
                (MdPara (MdStr "Run make deferred-items to regenerate flagged debt from source." ∷ []) ∷ []) ∷
                []
              ) ∷
              []
  in renderDoc (record { blocks = MarkdownDoc.blocks mdoc ++ˡ extra ; meta = MarkdownDoc.meta mdoc })

deferredTrackingMarkdown : String
deferredTrackingMarkdown =
  renderDoc (pandocDocToMarkdown deferredTrackingDoc)

readmeContent : String
readmeContent = readmeMarkdown

contributingContent : String
contributingContent = contributingMarkdown

navigationContent : String
navigationContent = navigationMarkdown

roadmapContent : String
roadmapContent = roadmapMarkdown

deferredTrackingContent : String
deferredTrackingContent = deferredTrackingMarkdown

frameworkInteroperabilityContent : String
frameworkInteroperabilityContent = renderDoc (pandocDocToMarkdown frameworkInteroperabilityDoc)

qualityFrameworkContent : String
qualityFrameworkContent = renderDoc (pandocDocToMarkdown qualityFrameworkDoc)

------------------------------------------------------------------------
-- IO operations (postulates with GHC bindings via adapters)
------------------------------------------------------------------------

postulate
  _>>=_ : {A B : Set} → IO A → (A → IO B) → IO B
  return : {A : Set} → A → IO A
  writeFile : String → String → IO ⊤
  putStrLn : String → IO ⊤

{-# COMPILE GHC _>>=_ = \_ _ -> (>>=) #-}
{-# COMPILE GHC return = \_ -> return #-}
{-# COMPILE GHC writeFile = writeFileAdapter #-}
{-# COMPILE GHC putStrLn = putStrLnAdapter #-}

------------------------------------------------------------------------
-- Main program: generate all three markdown files
------------------------------------------------------------------------

main : IO ⊤
main = 
  writeFile "README.md" readmeContent >>= λ _ →
  putStrLn "✓ Generated README.md" >>= λ _ →
  writeFile "CONTRIBUTING.md" contributingContent >>= λ _ →
  putStrLn "✓ Generated CONTRIBUTING.md" >>= λ _ →
  writeFile "docs/NAVIGATION.md" navigationContent >>= λ _ →
  putStrLn "Generated docs/NAVIGATION.md" >>= λ _ →
  writeFile "docs/planning/ROADMAP.md" roadmapContent >>= λ _ →
  putStrLn "Generated docs/planning/ROADMAP.md" >>= λ _ →
  writeFile "docs/status/DEFERRED-TRACKING.md" deferredTrackingContent >>= λ _ →
  putStrLn "Generated docs/status/DEFERRED-TRACKING.md" >>= λ _ →
  writeFile "docs/architecture/FRAMEWORK-INTEROPERABILITY.md" frameworkInteroperabilityContent >>= λ _ →
  putStrLn "Generated docs/architecture/FRAMEWORK-INTEROPERABILITY.md" >>= λ _ →
  writeFile "docs/process/QUALITY-FRAMEWORK.md" qualityFrameworkContent >>= λ _ →
  putStrLn "Generated docs/process/QUALITY-FRAMEWORK.md" >>= λ _ →
  putStrLn "Documentation generation complete!"
