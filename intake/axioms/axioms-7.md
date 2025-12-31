Below is a concrete, “you can paste this into Agda” answer set. I’m going to be opinionated where you asked for preference (R/N, movie encoding, d₁/d₂ meaning, metrics/gates), and I’ll keep everything compatible with your “axioms are parameters; theorems are generated” invariant.

---

## 1) Solvable-interface spec: Kit / Face / Solver

### 1.1 Core types (Agda)

This is the minimal, canonical interface I recommend:

```agda
module Infrastructure.Axiom.Solvable where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)

-- A typed path algebra (free category, or a quotient, or a concrete category)
record PathAlg {ℓV ℓP : Level} (V : Set ℓV) : Set (lsuc (ℓV ⊔ ℓP)) where
  field
    Path  : V → V → Set ℓP
    _∙_   : ∀ {a b c} → Path a b → Path b c → Path a c
    id    : ∀ {a} → Path a a

    assoc : ∀ {a b c d} (p : Path a b) (q : Path b c) (r : Path c d)
          → (p ∙ q) ∙ r ≡ p ∙ (q ∙ r)
    idl   : ∀ {a b} (p : Path a b) → id ∙ p ≡ p
    idr   : ∀ {a b} (p : Path a b) → p ∙ id ≡ p

open PathAlg public

-- A Face is a boundary: two parallel paths with same endpoints
record Face {ℓV ℓP : Level} {V : Set ℓV} (PA : PathAlg {ℓV} {ℓP} V)
            (a b : V) : Set (ℓV ⊔ ℓP) where
  field
    lhs rhs : Path PA a b

-- A “framed face” packages endpoints so Face can be extracted from Kit without extra args
record FramedFace {ℓV ℓP : Level} {V : Set ℓV} (PA : PathAlg {ℓV} {ℓP} V)
  : Set (lsuc (ℓV ⊔ ℓP)) where
  field
    a b   : V
    face  : Face PA a b

-- Solver takes a Kit and produces proof that the boundary commutes
record Solver {ℓV ℓP ℓK : Level} {V : Set ℓV}
              (PA : PathAlg {ℓV} {ℓP} V)
              (Kit : Set ℓK)
              : Set (lsuc (ℓV ⊔ ℓP ⊔ ℓK)) where
  field
    boundary : Kit → FramedFace PA
    solve    : (k : Kit) →
               let ff = boundary k in
               Face.lhs (FramedFace.face ff) ≡ Face.rhs (FramedFace.face ff)
```

**Why this is canonical for your project**

* The “axiom” is **`boundary : Kit → FramedFace`**. That’s the *problem statement*, not a truth.
* The “theorem generator” is **`solve : (k : Kit) → lhs ≡ rhs`**.
* No postulates. All coherence is *demanded* via a kit.

### 1.2 Optional: Partial/diagnostic solver (Maybe or obligations)

For auditing/gaps you’ll want a solver that can fail with an explicit obligation:

```agda
data Obligation : Set where
  MissingTriangle : Obligation
  MissingWhisker  : Obligation
  -- extend as needed

record Solver? {ℓV ℓP ℓK : Level} {V : Set ℓV}
               (PA : PathAlg V)
               (Kit : Set ℓK)
               : Set (lsuc (ℓV ⊔ ℓP ⊔ ℓK)) where
  field
    boundary : Kit → FramedFace PA
    solve?   : (k : Kit) → (Face.lhs (FramedFace.face (boundary k)) ≡
                            Face.rhs (FramedFace.face (boundary k))) ⊎ Obligation
```

This is the “adequacy as solvability” dial.

### 1.3 Canonical Haskell sketch (optional, matches the above)

```hs
data Face v p = Face { lhs :: p, rhs :: p, a :: v, b :: v }

data Solver v p kit = Solver
  { boundary :: kit -> Face v p
  , solve    :: kit -> Proof -- or Either Obligation Proof
  }
```

---

## 2) Triangulated coherence: exact data + “movie” encoding + roadmap indexing

### 2.1 What data I want for pentagon/hexagon

**Pentagon (associator coherence)** as *fan triangulation* from a chosen apex vertex `v0`:

* vertices `v0 v1 v2 v3 v4`
* boundary paths:

  * `short : Path v0 v4` (2 edges)
  * `long  : Path v0 v4` (3 edges)
* diagonals:

  * one diagonal `d13 : Path v1 v3` (fan choice)
* triangle fillers (local 2-cells):

  * `t1 : (e01∙d13) ≡ (e01∙e12∙e23)` style, but kept abstract as `Face` proofs
  * `t2 : (d13∙e34) ≡ (e23∙e34)` etc.

**Hexagon (braiding coherence)** similarly as a fan triangulation of a 6-cycle into 4 triangles using diagonals from an apex.

The key is: **the Kit provides triangles + diagonals**, solver glues them.

### 2.2 “Movie” / homotopy representation in Agda

I recommend representing a “movie” as a **2D compositional derivation** in a free 2-category of faces:

```agda
module Infrastructure.Movie where
open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)

open Infrastructure.Axiom.Solvable

-- A 2-cell: equality between two paths
record Cell2 {ℓV ℓP : Level} {V : Set ℓV}
             (PA : PathAlg V)
             {a b : V}
             (p q : Path PA a b)
             : Set (ℓV ⊔ ℓP) where
  field proof : p ≡ q

-- “Movie” is a compositional proof object built from primitives
data Movie {ℓV ℓP : Level} {V : Set ℓV} (PA : PathAlg V)
          : ∀ {a b} → (p q : Path PA a b) → Set (lsuc (ℓV ⊔ ℓP)) where
  step  : ∀ {a b} {p q : Path PA a b} → Cell2 PA p q → Movie PA p q
  vcomp : ∀ {a b} {p q r : Path PA a b} → Movie PA p q → Movie PA q r → Movie PA p r

-- interpretation
interp : ∀ {ℓV ℓP} {V : Set ℓV} {PA : PathAlg V} {a b}
       → {p q : Path PA a b} → Movie PA p q → p ≡ q
interp (step c)      = Cell2.proof c
interp (vcomp m n)   = Agda.Builtin.Equality.trans (interp m) (interp n)
```

You can extend `Movie` with whiskering/horizontal composition (very useful for naturality), but this is the minimal “homotopy trace”.

### 2.3 Octahedron axiom (what data I expect)

If you mean the **octahedron** in triangulated categories: that’s a *different* universe (distinguished triangles + cones). If instead you mean the **3D coherence polytope** arising from interleaving associator/braiding/naturality (often “cube/octahedron-like” in rewriting presentations), then my expectation is:

* you’ll represent it as a **3-cell**: equality between two **Movies** (i.e., homotopy between homotopies)
* data: a triangulation of the 3D boundary into tetrahedra (3-simplices), where each simplex is a local 3-cell generator

I’d encode that as `Movie3 : Movie p q → Movie p q → Set` with generators + composition.

### 2.4 Roadmap indexing (how I’d index it in CIM)

For each coherence polytope, add a RoadmapItem keyed by **arity + cell-dimension + generator basis**:

* `Coh/Assoc/K4/Triangulated` (pentagon derived from triangles)
* `Coh/Braid/Hexagon/Triangulated`
* `Coh/3Cell/Octahedron/Tetrahedralization` (if/when you go 3D)

Each item should list:

* module path(s) implementing `Kit`, `boundary`, `solve`
* proof obligations (what primitives the kit must provide)
* audits (`d²=0` checks, confluence, etc.)

---

## 3) Syzygy modules / bicategorical correspondence (R, N) + example derivation

### 3.1 My preferred mapping (what are R and N?)

I recommend:

* **R** = *Relations / Rewrite generators* (1-cells)
  “moves” / “steps”: associator-steps, braid-steps, naturality steps
* **N** = *Null-homotopies / 2-cell generators*
  triangles, squares, and “primitive commuting” cells used for triangulation

Then:

* a **Face** is a 1-cycle in the free category on R
* a **Solver** builds a 2-chain in the free 2-category generated by N whose boundary is that 1-cycle

This is exactly syzygy language:

* syzygies are relations among relations → your 2-cells are syzygies among step generators.

### 3.2 Example: a triangulated pentagon as syzygy composition

Let your pentagon boundary be the difference of two paths:

* `pShort : v0 → v4` (2 steps)
* `pLong  : v0 → v4` (3 steps)

Define the 1-cycle (in groupoid-ish notation) as:

* `cycle = pShort · (pLong)^{-1}`

Your triangulation kit provides 2-cells (in N) whose boundaries cancel internal edges and leave exactly `cycle`.

So the derivation is:

* `d₂(Σ triangles) = cycle`
* then `d₁(d₂(...)) = 0` automatically, yielding the audit.

### 3.3 The chain complex you want to materialize

At minimum (mod 2 coefficients is easiest for auditing):

* `F0` = free vector space on vertices (0-cells)
* `F1` = free vector space on directed edges/steps (R-instances)
* `F2` = free vector space on 2-cells (N-instances, e.g., triangles)

Maps:

* `d1 : F1 → F0` with `d1(e : a→b) = b - a`
* `d2 : F2 → F1` with `d2(triangle) = (top path) - (bottom path)` expanded into edges

In GF(2), “-” becomes XOR; orientations handled by choosing consistent edge directions.

---

## 4) SPPF / differential Earley: auditor algorithm sketch, invariants, d₁/d₂

### 4.1 Intended objects

Your Earley/SPPF world gives you:

* **Items**: `(i, A → α • β, j)` (standard Earley)
* **SPPF nodes**: packed nodes representing ambiguity (two parents)
* **Derivations**: hyperedges (rule applications, completions)

We map this to a cell complex:

* **0-cells (C0)**: chart states / node ids / spans
  (pick one canonical: I recommend *SPPF node IDs* or *Earley items*)
* **1-cells (C1)**: derivation steps (predict/scan/complete transitions)
  edges between states/items/nodes
* **2-cells (C2)**: commuting diamonds that encode “two ways to derive the same consequence”
  e.g., alternative completion orders that should be confluent, or packed-node joins.

### 4.2 What are d₁ and d₂ in code?

* `d₁` (edge boundary): takes an edge `u→v` to `(v - u)`
* `d₂` (diamond boundary): takes a 2-cell (diamond) to XOR of its perimeter edges

**Invariant**: `d₁ ∘ d₂ = 0`
This means every declared commuting diamond is actually a closed 1-cycle.

### 4.3 Minimal auditor pseudocode

```python
# Inputs:
#  nodes: list[NodeID]
#  edges: list[(src, dst, label)]
#  cells2: list[(u, a, t, b, v)]  # a diamond u->a->v and u->b->v, or u->a->t->v etc.

# Build index
node_index = {n:i for i,n in enumerate(nodes)}
edge_index = {(s,d,l):k for k,(s,d,l) in enumerate(edges)}

def d1(edge_k):
    s,d,_ = edges[edge_k]
    return { node_index[d]:1, node_index[s]:1 }  # GF(2): XOR endpoints

def d2(cell):
    # boundary = path1 XOR path2 (expanded to edges)
    # simplest diamond: u->a->v and u->b->v
    u,a,v,b,_ = cell
    e1 = edge_index[(u,a,"")]; e2 = edge_index[(a,v,"")]
    e3 = edge_index[(u,b,"")]; e4 = edge_index[(b,v,"")]
    return {e1:1, e2:1, e3:1, e4:1}

# audit d1(d2)=0
for cell in cells2:
    chain1 = d2(cell)              # in C1
    boundary = {}
    for e_k in chain1:
        bd = d1(e_k)               # in C0
        xor_into(boundary, bd)
    assert boundary == {}          # GF(2) zero vector
```

### 4.4 Acceptance condition

You can use this auditor in two ways:

1. **Structural sanity**: ensure your declared 2-cells are coherent (`d²=0`)
2. **Obstruction detector**: if you *expect* a commuting diamond but cannot generate it from the parse forest, you’ve found a “hole” (missing witness / missing rule / wrong factoring)

---

## 5) Nedge / AJP: metrics + gate semantics (confidence, plausibility, leaf-first, throttling)

I’m going to define these in the style you’ve been converging toward: *measure as structure*, not prose.

### 5.1 Core metrics (minimal, compositional)

Let a hypothesis node `h` have evidential supports `{e_i}` with weights.

Define:

* **Confidence** `Conf(h)` = witnessed derivability strength
  (how much of the claim is backed by explicit constructions/witnesses)

* **Plausibility** `Plaus(h)` = non-refuted support mass
  (how much survives adversarial pressure; includes compatible ambiguity)

* **Leaf-first resolution pressure** `Leaf(h)` = fraction of unresolved obligations that occur at minimal depth
  (how much “cheap” progress is available without lifting dimension)

A simple, compositional choice (works with your “witness/obligation” pipeline):

```text
Conf(h) = |W(h)| / (|W(h)| + |O(h)| + ε)
Plaus(h)= |W(h)| / (|W(h)| + |R(h)| + ε)
Leaf(h) = |O_leaf(h)| / (|O(h)| + ε)
```

Where:

* `W(h)` = witnessed subclaims supporting h
* `O(h)` = outstanding obligations (missing witnesses)
* `R(h)` = rebuttals / contradictions affecting h
* `O_leaf(h)` = obligations at minimal depth (fast to discharge)

These are deliberately “count-based”; later you can replace counts with your G-value calculus / harmonic mean etc. The important part is that they’re monotone and compositional.

### 5.2 Gate semantics for phase intention / metabolic throttling (AJP)

Define a gate function that chooses mode/phase based on metrics:

* **Intention phase**: what kind of work to do next
  (expand space, discharge obligations, reduce ambiguity, etc.)
* **Metabolic throttle**: how much branching / exploration you permit

Example decision policy:

```text
if Conf low and Leaf high:
    phase = "DISCHARGE_LEAVES"   # cheapest witnesses first
    throttle = "LOW_BRANCH"
elif Plaus low or contradictions high:
    phase = "ADVERSARIAL_REPAIR" # add structure, do not collapse
    throttle = "MED_BRANCH"
elif blocked (no progress) or Leaf low:
    phase = "LIFT_DIMENSION"     # A10: expand space
    throttle = "HIGH_BRANCH"
else:
    phase = "NORMALIZE"          # consolidate, dedup, canonicalize
    throttle = "LOW_BRANCH"
```

This matches your FAST/FULL logic, but in metric form (AJP-friendly).

### 5.3 Phase intention as a typed interface (Agda sketch)

```agda
data Phase : Set where
  DISCHARGE_LEAVES ADVERSARIAL_REPAIR LIFT_DIMENSION NORMALIZE : Phase

record Metrics : Set where
  field conf plaus leaf : ℚ  -- or your 4VL/G-value type

decide : Metrics → Phase
decide m with Metrics.conf m | Metrics.plaus m | Metrics.leaf m
... | c | p | l = {!!} -- implement thresholds / lattice decision
```

(Use your own numeric domain; the interface is what matters.)

---

# The “worked example” you asked for: encoding pentagon via triangles

Here’s the canonical pattern, abstractly, in Agda terms:

```agda
module Infrastructure.Polytopes.PentagonFromTriangles
  {ℓV ℓP : Level} {V : Set ℓV} (PA : PathAlg V)
  where

open PathAlg PA

-- vertices
variable v0 v1 v2 v3 v4 : V

-- edges
variable e01 : Path v0 v1
variable e12 : Path v1 v2
variable e23 : Path v2 v3
variable e34 : Path v3 v4
variable e13 : Path v1 v3  -- diagonal from triangulation

-- boundary paths
pShort : Path v0 v4
pShort = e01 ∙ e13 ∙ e34

pLong : Path v0 v4
pLong = e01 ∙ e12 ∙ e23 ∙ e34

-- triangle fillers are local 2-cells that relate diagonal composites to boundary composites
Triangle1 : Set (ℓV ⊔ ℓP)
Triangle1 = (e13 ≡ e12 ∙ e23)

-- if Triangle1 holds, then whisker it with e01 on left and e34 on right
whisker : ∀ {a b c d} → (p : Path a b) → (q : Path c d) →
          ∀ {m n} → (r s : Path b c) → (r ≡ s) → (p ∙ r ∙ q ≡ p ∙ s ∙ q)
whisker p q r s eq =
  -- proof via assoc + congruence (left as exercise / standard lemma)
  {!!}

pentagon-from-triangle : Triangle1 → pShort ≡ pLong
pentagon-from-triangle t1 =
  -- use whisker with p=e01 and q=e34
  whisker e01 e34 e13 (e12 ∙ e23) t1
```

In practice you’ll package `Triangle1` as a `Cell2`/`Face` proof, and the solver becomes “apply whiskering + assoc rewrites.” If you triangulate the pentagon into **two** diagonals and **two** triangles, you compose two such steps (your earlier fan kit).

---

## If you want “canonical snippets” already in your repo

Given your earlier Infrastructure work, the “canonical” pieces in the codebase are exactly the ones matching the signatures above:

* `PathAlg` / `Face` / `FramedFace` / `Solver`
* `PentagonFan` / `HexagonFan` kits
* `pentagon-from-fan` / `hexagon-from-fan`
* `Movie` as compositional 2-cell traces (optional but recommended)

If you paste me the current exact module names you settled on in `Infrastructure/`, I can adapt the above into “imports + exact identifiers” so it compiles against your tree with no renaming.

---

If you want one extra deliverable next: I can write the **Roadmap-index entries** (CIM `RoadmapItem`s) for these five blocks with dependency edges and explicit “missing witness” obligations (what kits must provide), so the roadmap becomes an executable spec of adequacy.
