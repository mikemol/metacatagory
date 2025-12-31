#!/usr/bin/env python3
"""
RoadmapTraverser: Agentic processor for roadmap steps.

Phase 3: Agentic Roadmap Traversal
- Process each RoadmapStep automatically
- Generate implementation checklists
- Track progress and dependencies
- Enable parallel execution
"""

import json
import os
from typing import Dict, List, Set, Tuple
from dataclasses import dataclass, asdict
from enum import Enum
from pathlib import Path

class Status(Enum):
    """Roadmap step status."""
    NOT_STARTED = "not-started"
    IN_PROGRESS = "in-progress"
    BLOCKED = "blocked"
    COMPLETED = "completed"
    DEFERRED = "deferred"

@dataclass
class RoadmapStep:
    """Representation of a roadmap step."""
    gp_id: str
    title: str
    provenance: str
    summary: str
    implication: str
    status: Status
    target_module: str
    keywords: List[str]
    dependencies: List[str] = None
    related_nodes: List[str] = None
    
    def __post_init__(self):
        if self.dependencies is None:
            self.dependencies = []
        if self.related_nodes is None:
            self.related_nodes = []
    
    def to_dict(self):
        """Convert to dictionary."""
        d = asdict(self)
        d['status'] = self.status.value
        return d

@dataclass
class StepChecklist:
    """Checklist for a roadmap step."""
    step_id: str
    title: str
    status: Status
    subtasks: List[Dict]
    blocked_by: List[str]
    blocking: List[str]
    estimated_effort: str
    priority: int
    
    def to_dict(self):
        """Convert to dictionary."""
        return {
            'step_id': self.step_id,
            'title': self.title,
            'status': self.status.value,
            'subtasks': self.subtasks,
            'blocked_by': self.blocked_by,
            'blocking': self.blocking,
            'estimated_effort': self.estimated_effort,
            'priority': self.priority,
        }

class RoadmapTraverser:
    """Agent for traversing and processing roadmap steps."""
    
    def __init__(self, metadata_path: str):
        """Initialize traverser with metadata."""
        self.metadata_path = metadata_path
        self.steps: Dict[str, RoadmapStep] = {}
        self.checklists: Dict[str, StepChecklist] = {}
        self.dependency_graph: Dict[str, Set[str]] = {}
        
        self._load_metadata()
        self._build_dependency_graph()
    
    def _load_metadata(self):
        """Load metadata from JSON."""
        with open(self.metadata_path, 'r') as f:
            data = json.load(f)
        
        for gp_id, meta in data['files'].items():
            step = RoadmapStep(
                gp_id=gp_id,
                title=meta['title'],
                provenance=meta.get('provenance', gp_id),
                summary=meta.get('summary', ''),
                implication='Extends the roadmap',
                status=Status.NOT_STARTED,
                target_module='src/agda/Plan/CIM/Polytopes.agda',
                keywords=meta.get('keywords', []),
            )
            self.steps[gp_id] = step
    
    def _build_dependency_graph(self):
        """Build dependency graph from steps."""
        # Initialize graph
        for gp_id in self.steps:
            self.dependency_graph[gp_id] = set()
        
        # Add edges based on keywords and titles
        for gp_id, step in self.steps.items():
            # Infer dependencies from category (simple heuristic)
            # E.g., GP501 (Polytope) might depend on GP300 (Topological)
            num = int(gp_id.replace('GP', ''))
            
            # High-level items depend on lower-level foundations
            if 700 <= num < 800:  # Analysis items
                # Depend on foundational items
                for dep_id in self.steps:
                    dep_num = int(dep_id.replace('GP', ''))
                    if dep_num < 200 and dep_num > 0:
                        self.dependency_graph[gp_id].add(dep_id)
            elif 500 <= num < 600:  # Polytope items
                # Depend on geometric and topological items
                for dep_id in self.steps:
                    dep_num = int(dep_id.replace('GP', ''))
                    if 200 <= dep_num < 400:
                        self.dependency_graph[gp_id].add(dep_id)
    
    def analyze_step(self, gp_id: str) -> StepChecklist:
        """Analyze a step and generate checklist."""
        step = self.steps[gp_id]
        
        # Generate subtasks
        subtasks = [
            {
                'id': f'{gp_id}_req',
                'title': 'Clarify Requirements',
                'status': 'not-started',
                'effort': '2h'
            },
            {
                'id': f'{gp_id}_design',
                'title': 'Design Solution',
                'status': 'not-started',
                'effort': '4h'
            },
            {
                'id': f'{gp_id}_impl',
                'title': 'Implementation',
                'status': 'not-started',
                'effort': '8h'
            },
            {
                'id': f'{gp_id}_test',
                'title': 'Testing & Verification',
                'status': 'not-started',
                'effort': '4h'
            },
            {
                'id': f'{gp_id}_review',
                'title': 'Code Review & Refinement',
                'status': 'not-started',
                'effort': '2h'
            },
        ]
        
        # Determine priority (higher number = higher priority)
        # Lower GP numbers and analysis phase get higher priority
        num = int(gp_id.replace('GP', ''))
        if 700 <= num < 900:  # Analysis and unified phases
            priority = 8
        elif 100 <= num < 300:  # Foundational and structural
            priority = 7
        elif 300 <= num < 500:  # Geometric and topological
            priority = 6
        else:
            priority = 5
        
        checklist = StepChecklist(
            step_id=gp_id,
            title=step.title,
            status=step.status,
            subtasks=subtasks,
            blocked_by=list(self.dependency_graph.get(gp_id, set())),
            blocking=[k for k, v in self.dependency_graph.items() if gp_id in v],
            estimated_effort='20h',
            priority=priority,
        )
        
        self.checklists[gp_id] = checklist
        return checklist
    
    def get_ready_steps(self) -> List[str]:
        """Get steps that are ready to execute (dependencies satisfied)."""
        ready = []
        completed = {gp_id for gp_id, step in self.steps.items() 
                    if step.status == Status.COMPLETED}
        
        for gp_id, step in self.steps.items():
            if step.status == Status.NOT_STARTED:
                deps = self.dependency_graph.get(gp_id, set())
                if deps.issubset(completed):
                    ready.append(gp_id)
        
        return sorted(ready)
    
    def get_critical_path(self) -> List[str]:
        """Get the critical path (longest dependency chain)."""
        # Simple topological sort to find longest path
        visited = set()
        path_lengths = {}
        
        def dfs(node: str) -> int:
            if node in path_lengths:
                return path_lengths[node]
            
            deps = self.dependency_graph.get(node, set())
            if not deps:
                path_lengths[node] = 1
                return 1
            
            max_dep_length = max(dfs(d) for d in deps) if deps else 0
            path_lengths[node] = max_dep_length + 1
            return path_lengths[node]
        
        # Calculate path lengths
        for gp_id in self.steps:
            dfs(gp_id)
        
        # Find nodes in longest path
        max_length = max(path_lengths.values()) if path_lengths else 0
        critical_nodes = [gp_id for gp_id, length in path_lengths.items() 
                         if length == max_length]
        
        return sorted(critical_nodes, key=lambda x: int(x.replace('GP', '')))
    
    def generate_execution_plan(self) -> Dict:
        """Generate execution plan with parallelization info."""
        ready = self.get_ready_steps()
        critical = self.get_critical_path()
        
        plan = {
            'total_steps': len(self.steps),
            'ready_steps': ready,
            'ready_count': len(ready),
            'critical_path': critical,
            'critical_path_length': len(critical),
            'phases': []
        }
        
        # Identify execution phases
        completed = set()
        phase_num = 1
        
        while len(completed) < len(self.steps):
            # Find executable steps
            executable = []
            for gp_id in self.steps:
                if gp_id not in completed:
                    deps = self.dependency_graph.get(gp_id, set())
                    if deps.issubset(completed):
                        executable.append(gp_id)
            
            if not executable:
                break
            
            # Sort by priority
            executable.sort(key=lambda x: (
                int(x.replace('GP', '')) // 100,  # Category
                int(x.replace('GP', ''))  # Item number
            ))
            
            plan['phases'].append({
                'phase': phase_num,
                'items': executable,
                'count': len(executable),
                'parallelizable': len(executable) > 1,
                'estimated_duration': f'{len(executable) * 20}h (parallel)',
            })
            
            completed.update(executable)
            phase_num += 1
        
        plan['total_phases'] = len(plan['phases'])
        return plan
    
    def generate_report(self, output_path: str):
        """Generate comprehensive traversal report."""
        # Analyze all steps
        for gp_id in self.steps:
            self.analyze_step(gp_id)
        
        # Get critical metrics
        ready = self.get_ready_steps()
        critical = self.get_critical_path()
        plan = self.generate_execution_plan()
        
        report = {
            'metadata': {
                'total_steps': len(self.steps),
                'status_breakdown': {
                    'not_started': sum(1 for s in self.steps.values() 
                                      if s.status == Status.NOT_STARTED),
                    'in_progress': sum(1 for s in self.steps.values() 
                                      if s.status == Status.IN_PROGRESS),
                    'blocked': sum(1 for s in self.steps.values() 
                                  if s.status == Status.BLOCKED),
                    'completed': sum(1 for s in self.steps.values() 
                                    if s.status == Status.COMPLETED),
                },
            },
            'ready_steps': ready,
            'critical_path': critical,
            'execution_plan': plan,
            'steps': {gp_id: checklist.to_dict() 
                     for gp_id, checklist in self.checklists.items()},
        }
        
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        with open(output_path, 'w') as f:
            json.dump(report, f, indent=2)
        
        return report

def main():
    """Main execution."""
    print("=" * 70)
    print("PHASE 3: AGENTIC ROADMAP TRAVERSAL")
    print("=" * 70)
    
    metadata_path = 'build/ingested_metadata.json'
    output_path = 'build/roadmap_traversal_report.json'
    
    # Initialize traverser
    print("\n📊 Initializing RoadmapTraverser...")
    traverser = RoadmapTraverser(metadata_path)
    print(f"   ✓ Loaded {len(traverser.steps)} steps")
    
    # Generate report
    print("\n📋 Generating execution plan...")
    report = traverser.generate_report(output_path)
    
    # Display summary
    print(f"\n{'─' * 70}")
    print("ROADMAP TRAVERSAL SUMMARY")
    print(f"{'─' * 70}")
    print(f"\nTotal Steps: {report['metadata']['total_steps']}")
    print(f"Ready to Execute: {len(report['ready_steps'])}")
    print(f"Critical Path Length: {report['execution_plan']['critical_path_length']}")
    print(f"Total Phases: {report['execution_plan']['total_phases']}")
    
    print(f"\nStatus Breakdown:")
    for status, count in report['metadata']['status_breakdown'].items():
        print(f"  • {status.replace('_', ' ').title()}: {count}")
    
    print(f"\nReady Steps (can execute now):")
    for step_id in report['ready_steps'][:10]:
        print(f"  • {step_id}")
    if len(report['ready_steps']) > 10:
        print(f"  ... and {len(report['ready_steps']) - 10} more")
    
    print(f"\nCritical Path (longest dependency chain):")
    for step_id in report['critical_path'][:5]:
        print(f"  • {step_id}")
    if len(report['critical_path']) > 5:
        print(f"  ... and {len(report['critical_path']) - 5} more")
    
    print(f"\nExecution Phases: {report['execution_plan']['total_phases']}")
    for phase in report['execution_plan']['phases'][:3]:
        print(f"  Phase {phase['phase']}: {phase['count']} items "
              f"({'parallelizable' if phase['parallelizable'] else 'sequential'})")
    
    print(f"\n✓ Full report saved to: {output_path}")
    print(f"{'─' * 70}\n")

if __name__ == '__main__':
    main()
