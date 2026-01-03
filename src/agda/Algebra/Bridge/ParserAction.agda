{-# OPTIONS --without-K #-}

-- | Bridge between algebraic groups and the parser SPPF layout.
--   This module fulfills Operation B by postulating the `RotationalTransport`
--   group action that links the unit circle (U₁) to the `SPPFNode` space
--   terms encoded in `Plan.CIM.RoadmapSPPF`.  It documents the intent from
--   GP01 (RotationalTransport) while keeping the bridge lightweight enough to
--   slot into the existing algebraic hierarchy.

module Algebra.Bridge.ParserAction where

open import Core
open import Algebra.Foundation
open import Algebra.Groups.Structure as AGS
open import Plan.CIM.RoadmapSPPF using (SPPFNode)
open import Metamodel as M

-- | A shared identifier for the parser action carrier.
SPPFNodeSpace : M.Identifier
SPPFNodeSpace = M.mkId "SPPFNodeSpace"

-- | A generic element of the parser action carrier.
SPPFNodeElement : M.Identifier
SPPFNodeElement = M.mkId "SPPFNodeElement"

-- | U₁ as the rotational symmetry group that moves parser nodes through
--   the SymNum/RoPE-inspired phase space.
postulate
  U1 : GroupDeclaration

-- | RotationalTransport is the planned GroupAction linking U₁ → SPPFNode.
postulate
  RotationalTransport : AGS.GroupAction U1 SPPFNodeSpace

-- | Anchor the action to an abstract SPPF node so tooling infers the orbit.
postulate
  RotationalTransportOrbit : AGS.Orbit U1 SPPFNodeSpace RotationalTransport SPPFNodeElement

-- | Core theorems that certify the group action (orbits, stabilizers, etc.).
ParserActionProof : M.Identifier
ParserActionProof = AGS.GroupActionCoreTheorems U1

-- | Helpful aliases for downstream consumers.
rotationalTransportAction : M.Identifier
rotationalTransportAction = AGS.GroupAction.action RotationalTransport

rotationalTransportIdentity : M.Identifier
rotationalTransportIdentity = AGS.GroupAction.identityAction RotationalTransport

rotationalTransportCompatibility : M.Identifier
rotationalTransportCompatibility = AGS.GroupAction.compatibilityAction RotationalTransport

-- | Reference the actual SPPF node type (for documentation/IDE cross-links).
postulate
  parserActionNode : SPPFNode
