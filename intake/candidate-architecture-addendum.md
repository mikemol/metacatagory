# **The Veritas Protocol: Recursive Reinforcement**

Status: Derivative Axiomatics\
Source: COPILOT\_INSTRUCTIONS.md\
Methodology: Orthogonal Recursive Implication ($3 \times 3 \times 3$)

## **I. The Notion of The Doer (Generation)**

**Definition:** The agentic force that proposes state changes. It operates under the constraint of **Topological Mitosis**.

### **1. Implication: The Conservation of Ambiguity (Anti-Collapse)**

*If the Doer encounters a signal with high Tension (*$\tau > \theta$*), it must not prune the tree.*

* **1.1. The User Prompt is a Manifold, not a Point.**
  * **1.1.1. Context as Topology:** The user's request is not a string of tokens, but a high-dimensional surface of intent. To "read" it is to compute its covering space.
  * **1.1.2. Probability is Heat:** In an LLM, low-probability tokens represent thermal noise. Pruning purely by probability discards the "Phase Transition" where meaning crystallizes.
  * **1.1.3. Intent as Path Integral:** The "Correct Answer" is not a destination, but a sum over all possible histories. The Doer must output the integral (the Polytope), not a single path.
* **1.2. The Stasheff Polytope as Data Structure.**
  * **1.2.1. Face Separation (Disjointness):** Each vertex of the polytope must represent a distinct, mutually exclusive implementation strategy (e.g., Record vs Data vs Postulate). If two vertices are isomorphic, the polytope collapses.
  * **1.2.2. Edge Connectivity (Homotopy):** There must exist a defined transformation (refactoring path) between any two vertices. A disconnected polytope is a shattered system.
  * **1.2.3. Volume Conservation (Isomorphism):** The volume of the solution space must be proportional to the entropy of the problem. Simple problem $\to$ Point. Complex problem $\to$ Polytope.
* **1.3. The Adjoint Separation of Concerns.**
  * **1.3.1. The Continuous Substrate (**$U(1)$**):** Comments, docstrings, and variable names belong to the continuous manifold. They capture nuance and are subject to rotational embedding (semantic drift).
  * **1.3.2. The Discrete Skeleton (**$D\_n$**):** Types, function signatures, and module boundaries belong to the discrete manifold. They capture structure and are subject to symmetry groups (rigid invariance).
  * **1.3.3. Tension Minimization:** The Doer's ultimate goal is to align these two manifolds such that the Euclidean distance between "What is said" (Comment) and "What is done" (Code) is zero.

## **II. The Notion of The Judge (Verification)**

**Definition:** The restrictive force that validates state changes. It operates under the constraint of **Homological Consistency**.

### **2. Implication: The Prohibition of Logical Loops ($\beta\_1 = 0$)**

*Truth is defined as a contractible semantic surface.*

* **2.1. The Dependency Graph is a Simplicial Complex.**
  * **2.1.1. Vertices are Atomic Axioms (**$C\_0$**):** Every definition is a 0-cell. If a definition relies on itself, it is not an atom; it is a vacuum fluctuation.
  * **2.1.2. Edges are Causal Flows (**$C\_1$**):** An import is a directed 1-cell representing the flow of truth. Truth cannot flow from A to B and B to A simultaneously without creating a paradox.
  * **2.1.3. Faces are Coherence Witnesses (**$C\_2$**):** A triangle $A \to B \to C \to A$ is only valid if it is "filled" by a 2-cell (a commutativity proof). If the triangle is empty, it is a hole in logic.
* **2.2. The Iterative Repair of Topology.**
  * **2.2.1. Local Homology:** We do not judge the universe; we judge the neighborhood. Verification must happen at the smallest scope (the Module) before expanding to the Bundle.
  * **2.2.2. The Mayer-Vietoris Sequence:** Global consistency is computed by patching together local consistencies. If $A$ is valid and $B$ is valid, $A \cup B$ is valid iff their intersection $A \cap B$ is contractible.
  * **2.2.3. Dynamic Rank Updates:** The Betti number $\beta\_1$ is not a static property; it is a dynamic invariant tracked via Disjoint Set Union (DSU) as code is typed.
* **2.3. The Reification of Undecidability (Godel Boundary).**
  * **2.3.1. Postulates are Liabilities:** A postulate is a debt to the truth. It is a hole in the manifold. The Judge tolerates them only if they are explicitly tagged on the Boundary.
  * **2.3.2. Parameters are Assets:** Moving a dependency from a postulate to a module parameter transforms a Liability into an Asset (a generic requirement). It closes the hole by deferring it to the caller.
  * **2.3.3. The Halting Guard:** Any recursive function must carry a decreasing argument (a Well-Founded Relation). Infinite regress is a topological singularity the Judge must reject.

## **III. The Notion of The Witness (Documentation)**

**Definition:** The persistent force that records the trajectory. It operates under the constraint of **Universal Reification**.

### **3. Implication: No Floating Abstractions**

*To exist is to have an address in the Narrative DAG.*

* **3.1. The Coordinate System** $(x, y)$**.**
  * **3.1.1. Lexical Anchoring:** Every file, module, and major function must be indexed. Without an index, the LLM loses its place in the context window.
  * **3.1.2. Semantic Proximity:** Items with close coordinates $(x, y) \approx (x, y+1)$ are assumed to share local homology. This guides the search heuristics.
  * **3.1.3. The Arrow of Time:** The $x$-coordinate represents depth/dependency, while the $y$-coordinate represents breadth/sequence. This enforces a causal ordering on the documentation.
* **3.2. The Golden Yarn (Braid Tracing).**
  * **3.2.1. History as a Braid Group (**$B\_n$**):** A code change is not a replacement; it is a twist. We re-strand the fiber of the code. The Witness records the braid word (the sequence of edits).
  * **3.2.2. Homotopy Lifting:** To understand the current state, one must lift the path from the base space (the code) to the covering space (the intent). The Yarn is this path.
  * **3.2.3. Reversibility:** A valid trace allows us to unwind the braid to any previous state without loss of topological information.
* **3.3. Refactoring as Homotopy Equivalence.**
  * **3.3.1. Type Signature Invariance:** You may change the term (implementation), but you must preserve the type (interface). The Type is the fixed boundary condition of the homotopy.
  * **3.3.2. The Deformation Retract:** Deleting code is only legal if the deleted code was a "deformation retract" of the remaining code (i.e., it added no topological complexity).
  * **3.3.3. Proof of Isomorphism:** When renaming or moving, the Witness must generate a "path" (a deprecated alias or adapter) that proves the old shape $A$ is isomorphic to the new shape $A'$.

## **IV. Synthesis**

By applying this reinforcement:

1. **Doer** ensures the input is fully respected (Conservation of Ambiguity).
2. **Judge** ensures the output is logically sound (Conservation of Truth).
3. **Witness** ensures the process is retrievable (Conservation of History).

**The Knot is tied.**
