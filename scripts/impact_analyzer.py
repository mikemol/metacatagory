#!/usr/bin/env python3
"""
Impact Analyzer - Phase 4 Cross-Reference Resolution
Analyzes the impact of changing roadmap items on Agda modules:
- Identifies which modules will be affected
- Computes cascading impact through dependencies
- Prioritizes work to minimize rework
"""

import json
from pathlib import Path
from typing import Dict, List, Set, Tuple
from dataclasses import dataclass, field
from collections import defaultdict


@dataclass
class ImpactAssessment:
    """Assessment of impact for a roadmap step."""
    step_id: str
    title: str
    primary_module: str
    
    # Direct impact
    modules_modified: List[str] = field(default_factory=list)
    
    # Cascading impact
    modules_affected: List[str] = field(default_factory=list)
    affected_count: int = 0
    
    # Reverse impact (what affects this module)
    dependencies: List[str] = field(default_factory=list)
    dependency_count: int = 0
    
    # Priority metrics
    impact_score: float = 0.0
    priority_rank: int = 0


class ImpactAnalyzer:
    """Analyzes impact of roadmap changes on module dependencies."""
    
    def __init__(self, workspace_root: str = "/home/mikemol/github/metacatagory"):
        self.workspace_root = workspace_root
        self.module_mappings = {}
        self.dependency_graph = {}
        self.reverse_dependencies = {}
        
    def load_data(self) -> None:
        """Load module mappings and dependency graph."""
        print("📥 Loading module mappings and dependency graph...")
        
        # Load module mappings
        mappings_path = Path(self.workspace_root) / "build/module_mappings.json"
        with open(mappings_path, 'r') as f:
            data = json.load(f)
            self.module_mappings = {
                m['step_id']: m for m in data['mappings']
            }
        
        # Load dependency graph
        graph_path = Path(self.workspace_root) / "build/dependency_graph.json"
        with open(graph_path, 'r') as f:
            graph_data = json.load(f)
            
            # Build dependency and reverse dependency maps
            for node in graph_data['nodes']:
                module = node['module']
                self.dependency_graph[module] = set(node['imports'])
                self.reverse_dependencies[module] = set(node['imported_by'])
        
        print(f"   ✓ Loaded {len(self.module_mappings)} roadmap mappings")
        print(f"   ✓ Loaded {len(self.dependency_graph)} module dependencies")
    
    def analyze_impact(self) -> Dict[str, ImpactAssessment]:
        """Analyze impact for all roadmap steps."""
        print("\n🎯 Analyzing impact for all roadmap steps...")
        
        assessments = {}
        
        for step_id, mapping in self.module_mappings.items():
            assessment = self._analyze_single_step(step_id, mapping)
            assessments[step_id] = assessment
        
        # Compute priority rankings
        self._compute_priorities(assessments)
        
        print(f"   ✓ Analyzed impact for {len(assessments)} steps")
        
        return assessments
    
    def _analyze_single_step(
        self,
        step_id: str,
        mapping: Dict
    ) -> ImpactAssessment:
        """Analyze impact for a single roadmap step."""
        primary_module = mapping['primary_module']
        related_modules = mapping['related_modules']
        
        # Modules directly modified
        modules_modified = [primary_module] + related_modules[:3]  # Top 3 related
        
        # Find all affected modules (cascading through reverse dependencies)
        modules_affected = self._find_cascading_impact(modules_modified)
        
        # Find dependencies (what this module depends on)
        dependencies = self._find_transitive_dependencies(modules_modified)
        
        # Calculate impact score
        impact_score = self._calculate_impact_score(
            len(modules_modified),
            len(modules_affected),
            len(dependencies)
        )
        
        return ImpactAssessment(
            step_id=step_id,
            title=mapping['title'],
            primary_module=primary_module,
            modules_modified=modules_modified,
            modules_affected=list(modules_affected),
            affected_count=len(modules_affected),
            dependencies=list(dependencies),
            dependency_count=len(dependencies),
            impact_score=impact_score
        )
    
    def _find_cascading_impact(
        self,
        modules: List[str],
        max_depth: int = 5
    ) -> Set[str]:
        """Find all modules affected by changes to given modules."""
        affected = set()
        visited = set()
        
        # BFS through reverse dependencies
        from collections import deque
        queue = deque([(m, 0) for m in modules])
        
        while queue:
            module, depth = queue.popleft()
            
            if module in visited or depth > max_depth:
                continue
            
            visited.add(module)
            
            # Add modules that import this one
            for importer in self.reverse_dependencies.get(module, []):
                affected.add(importer)
                queue.append((importer, depth + 1))
        
        return affected
    
    def _find_transitive_dependencies(
        self,
        modules: List[str],
        max_depth: int = 5
    ) -> Set[str]:
        """Find all transitive dependencies of given modules."""
        dependencies = set()
        visited = set()
        
        # BFS through dependencies
        from collections import deque
        queue = deque([(m, 0) for m in modules])
        
        while queue:
            module, depth = queue.popleft()
            
            if module in visited or depth > max_depth:
                continue
            
            visited.add(module)
            
            # Add modules this one imports
            for dep in self.dependency_graph.get(module, []):
                dependencies.add(dep)
                queue.append((dep, depth + 1))
        
        return dependencies
    
    def _calculate_impact_score(
        self,
        modules_modified: int,
        modules_affected: int,
        dependencies: int
    ) -> float:
        """Calculate impact score for prioritization."""
        # Higher score = higher priority to do early (fewer dependencies, more impact)
        
        # Weight factors
        modification_weight = 1.0
        cascading_weight = 2.0
        dependency_penalty = 0.5
        
        score = (
            modification_weight * modules_modified +
            cascading_weight * modules_affected -
            dependency_penalty * dependencies
        )
        
        return max(score, 0.0)
    
    def _compute_priorities(
        self,
        assessments: Dict[str, ImpactAssessment]
    ) -> None:
        """Compute priority rankings based on impact scores."""
        # Sort by impact score (descending) and dependency count (ascending)
        sorted_steps = sorted(
            assessments.values(),
            key=lambda a: (-a.impact_score, a.dependency_count)
        )
        
        # Assign ranks
        for rank, assessment in enumerate(sorted_steps, 1):
            assessment.priority_rank = rank
    
    def identify_high_impact_steps(
        self,
        assessments: Dict[str, ImpactAssessment],
        threshold: float = 5.0
    ) -> List[ImpactAssessment]:
        """Identify high-impact steps that affect many modules."""
        high_impact = [
            a for a in assessments.values()
            if a.impact_score >= threshold
        ]
        
        return sorted(high_impact, key=lambda a: a.impact_score, reverse=True)
    
    def identify_foundational_steps(
        self,
        assessments: Dict[str, ImpactAssessment],
        max_dependencies: int = 3
    ) -> List[ImpactAssessment]:
        """Identify foundational steps with few dependencies (should do first)."""
        foundational = [
            a for a in assessments.values()
            if a.dependency_count <= max_dependencies
        ]
        
        return sorted(foundational, key=lambda a: a.dependency_count)
    
    def find_critical_path_steps(
        self,
        assessments: Dict[str, ImpactAssessment]
    ) -> List[ImpactAssessment]:
        """Find steps on the critical path (high impact, high dependencies)."""
        # Load critical path from dependency graph
        graph_path = Path(self.workspace_root) / "build/dependency_graph.json"
        with open(graph_path, 'r') as f:
            graph_data = json.load(f)
            critical_modules = set(graph_data['critical_path'])
        
        # Find steps that modify critical path modules
        critical_steps = [
            a for a in assessments.values()
            if a.primary_module in critical_modules
        ]
        
        return sorted(critical_steps, key=lambda a: a.priority_rank)
    
    def generate_report(
        self,
        assessments: Dict[str, ImpactAssessment],
        output_file: str = "build/impact_analysis.json"
    ) -> None:
        """Generate impact analysis report."""
        print(f"\n📄 Generating impact analysis report...")
        
        # Identify special categories
        high_impact = self.identify_high_impact_steps(assessments)
        foundational = self.identify_foundational_steps(assessments)
        critical_path = self.find_critical_path_steps(assessments)
        
        # Build report
        report = {
            'metadata': {
                'total_steps': len(assessments),
                'high_impact_steps': len(high_impact),
                'foundational_steps': len(foundational),
                'critical_path_steps': len(critical_path),
                'timestamp': self._get_timestamp()
            },
            'assessments': [
                {
                    'step_id': a.step_id,
                    'title': a.title,
                    'primary_module': a.primary_module,
                    'modules_modified': a.modules_modified,
                    'modules_affected': a.modules_affected[:20],  # Limit for size
                    'affected_count': a.affected_count,
                    'dependencies': a.dependencies[:20],  # Limit for size
                    'dependency_count': a.dependency_count,
                    'impact_score': round(a.impact_score, 2),
                    'priority_rank': a.priority_rank
                }
                for a in sorted(assessments.values(), 
                              key=lambda x: x.priority_rank)
            ],
            'high_impact_steps': [
                {
                    'step_id': a.step_id,
                    'title': a.title,
                    'impact_score': round(a.impact_score, 2),
                    'affected_count': a.affected_count
                }
                for a in high_impact[:20]
            ],
            'foundational_steps': [
                {
                    'step_id': a.step_id,
                    'title': a.title,
                    'dependency_count': a.dependency_count,
                    'impact_score': round(a.impact_score, 2)
                }
                for a in foundational[:20]
            ],
            'critical_path_steps': [
                {
                    'step_id': a.step_id,
                    'title': a.title,
                    'primary_module': a.primary_module,
                    'priority_rank': a.priority_rank
                }
                for a in critical_path
            ],
            'execution_recommendation': self._generate_execution_plan(
                assessments, foundational, high_impact, critical_path
            )
        }
        
        # Write report
        output_path = Path(self.workspace_root) / output_file
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(output_path, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"   ✓ Report saved to: {output_file}")
        
        # Print summary
        self._print_summary(report)
    
    def _generate_execution_plan(
        self,
        all_assessments: Dict[str, ImpactAssessment],
        foundational: List[ImpactAssessment],
        high_impact: List[ImpactAssessment],
        critical_path: List[ImpactAssessment]
    ) -> Dict:
        """Generate recommended execution plan."""
        # Phase 1: Foundational (low dependencies)
        phase1_ids = set(a.step_id for a in foundational[:30])
        
        # Phase 2: High impact (affects many modules)
        phase2_ids = set(a.step_id for a in high_impact[:20]) - phase1_ids
        
        # Phase 3: Critical path items
        phase3_ids = set(a.step_id for a in critical_path) - phase1_ids - phase2_ids
        
        # Phase 4: Remaining
        all_ids = set(all_assessments.keys())
        phase4_ids = all_ids - phase1_ids - phase2_ids - phase3_ids
        
        return {
            'phases': [
                {
                    'phase': 1,
                    'name': 'Foundational Items',
                    'rationale': 'Low dependencies, can execute immediately',
                    'count': len(phase1_ids),
                    'items': sorted(phase1_ids)
                },
                {
                    'phase': 2,
                    'name': 'High Impact Items',
                    'rationale': 'Affect many modules, do early to minimize rework',
                    'count': len(phase2_ids),
                    'items': sorted(phase2_ids)
                },
                {
                    'phase': 3,
                    'name': 'Critical Path Items',
                    'rationale': 'On dependency critical path',
                    'count': len(phase3_ids),
                    'items': sorted(phase3_ids)
                },
                {
                    'phase': 4,
                    'name': 'Remaining Items',
                    'rationale': 'Lower priority items',
                    'count': len(phase4_ids),
                    'items': sorted(phase4_ids)
                }
            ]
        }
    
    def _get_timestamp(self) -> str:
        """Get current timestamp."""
        from datetime import datetime
        return datetime.now().isoformat()
    
    def _print_summary(self, report: Dict) -> None:
        """Print summary statistics."""
        print("\n" + "="*70)
        print("IMPACT ANALYSIS SUMMARY")
        print("="*70)
        
        meta = report['metadata']
        
        print(f"\nAnalysis Overview:")
        print(f"  Total Steps: {meta['total_steps']}")
        print(f"  High Impact Steps: {meta['high_impact_steps']}")
        print(f"  Foundational Steps: {meta['foundational_steps']}")
        print(f"  Critical Path Steps: {meta['critical_path_steps']}")
        
        print(f"\nTop 10 High-Impact Steps:")
        for item in report['high_impact_steps'][:10]:
            print(f"  {item['step_id']}: {item['affected_count']} modules affected (score: {item['impact_score']})")
        
        print(f"\nTop 10 Foundational Steps (fewest dependencies):")
        for item in report['foundational_steps'][:10]:
            print(f"  {item['step_id']}: {item['dependency_count']} dependencies")
        
        print(f"\nRecommended Execution Phases:")
        for phase in report['execution_recommendation']['phases']:
            print(f"  Phase {phase['phase']}: {phase['name']} ({phase['count']} items)")
            print(f"    → {phase['rationale']}")
        
        print("\n" + "="*70)


def main():
    """Main entry point."""
    print("="*70)
    print("PHASE 4: IMPACT ANALYZER")
    print("="*70)
    
    analyzer = ImpactAnalyzer()
    
    # Load data
    analyzer.load_data()
    
    # Analyze impact
    assessments = analyzer.analyze_impact()
    
    # Generate report
    analyzer.generate_report(assessments)
    
    print("\n✓ Impact analysis complete!")


if __name__ == '__main__':
    main()
