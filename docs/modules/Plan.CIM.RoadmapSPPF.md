---
module: Plan.CIM.RoadmapSPPF
kind: per-module
imports:
  - Agda.Builtin.String using (String; primStringAppend)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Maybe using (Maybe; just; nothing)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Plan.CIM.RoadmapIndex using (RoadmapItem; RoadmapAdapter; unifiedIndex)
---

# Module: Plan.CIM.RoadmapSPPF

**Source:** `src/agda/Plan/CIM/RoadmapSPPF.agda`

## Dependencies

- Agda.Builtin.String using (String; primStringAppend)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Maybe using (Maybe; just; nothing)
- Agda.Builtin.Bool using (Bool; true; false)
- Plan.CIM.RoadmapIndex using (RoadmapItem; RoadmapAdapter; unifiedIndex)
