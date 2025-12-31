---
module: Tests.EnrichmentChecklist
kind: per-module
imports:
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit using (⊤)
  - Core.Phase using (Bool; true; false)
  - Metamodel as M
  - Algebra.Foundation as AFo
  - Algebra.Enrichment as AE
  - Algebra.Groups.Abelian as AGA
  - Chapter1.Level1sub3 as C1S3
  - Chapter2.Level2sub6 as Enriched
  - Tests.ObligationAdapters as A
---

# Module: Tests.EnrichmentChecklist

**Source:** `src/agda/Tests/EnrichmentChecklist.agda`

## Dependencies

- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤)
- Core.Phase using (Bool; true; false)
- Metamodel as M
- Algebra.Foundation as AFo
- Algebra.Enrichment as AE
- Algebra.Groups.Abelian as AGA
- Chapter1.Level1sub3 as C1S3
- Chapter2.Level2sub6 as Enriched
- Tests.ObligationAdapters as A
