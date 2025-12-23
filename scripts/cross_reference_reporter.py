#!/usr/bin/env python3
"""
Cross-Reference Report Generator - Phase 4
Integrates all Phase 4 analyses into comprehensive report:
- Module mappings
- Dependency graphs
- Impact assessments
- Execution recommendations
"""

import json
from pathlib import Path
from typing import Dict, List
from datetime import datetime


class CrossReferenceReporter:
    """Generates comprehensive cross-reference report."""
    
    def __init__(self, workspace_root: str = "/home/mikemol/github/metacatagory"):
        self.workspace_root = workspace_root
        
    def load_all_data(self) -> Dict:
        """Load all Phase 4 analysis results."""
        print("📥 Loading all Phase 4 analysis data...")
        
        # Load module mappings
        with open(Path(self.workspace_root) / "build/module_mappings.json") as f:
            module_data = json.load(f)
        
        # Load dependency graph
        with open(Path(self.workspace_root) / "build/dependency_graph.json") as f:
            dependency_data = json.load(f)
        
        # Load impact analysis
        with open(Path(self.workspace_root) / "build/impact_analysis.json") as f:
            impact_data = json.load(f)
        
        # Load original roadmap traversal
        with open(Path(self.workspace_root) / "build/roadmap_traversal_report.json") as f:
            traversal_data = json.load(f)
        
        print("   ✓ All data loaded")
        
        return {
            'modules': module_data,
            'dependencies': dependency_data,
            'impact': impact_data,
            'traversal': traversal_data
        }
    
    def generate_integrated_report(
        self,
        data: Dict,
        output_file: str = "build/cross_reference_report.json"
    ) -> None:
        """Generate integrated cross-reference report."""
        print("\n📊 Generating integrated cross-reference report...")
        
        # Build integrated execution plan
        execution_plan = self._build_execution_plan(data)
        
        # Build module impact matrix
        impact_matrix = self._build_impact_matrix(data)
        
        # Compile report
        report = {
            'metadata': {
                'phase': 4,
                'name': 'Cross-Reference Resolution',
                'timestamp': datetime.now().isoformat(),
                'total_steps': data['modules']['metadata']['total_steps'],
                'total_modules': data['modules']['metadata']['total_modules'],
                'total_dependencies': data['dependencies']['metadata']['total_dependencies']
            },
            
            'summary': {
                'module_mappings': {
                    'total_steps': len(data['modules']['mappings']),
                    'high_confidence': sum(
                        1 for m in data['modules']['mappings'] 
                        if m['confidence'] >= 0.7
                    ),
                    'medium_confidence': sum(
                        1 for m in data['modules']['mappings'] 
                        if 0.4 <= m['confidence'] < 0.7
                    ),
                    'low_confidence': sum(
                        1 for m in data['modules']['mappings'] 
                        if m['confidence'] < 0.4
                    )
                },
                'dependency_structure': {
                    'total_modules': data['dependencies']['metadata']['total_modules'],
                    'total_edges': data['dependencies']['metadata']['total_dependencies'],
                    'layers': data['dependencies']['metadata']['dependency_layers'],
                    'critical_path_length': data['dependencies']['metadata']['critical_path_length'],
                    'cycles_detected': data['dependencies']['metadata']['cycles_detected']
                },
                'impact_analysis': {
                    'high_impact_steps': data['impact']['metadata']['high_impact_steps'],
                    'foundational_steps': data['impact']['metadata']['foundational_steps'],
                    'critical_path_steps': data['impact']['metadata']['critical_path_steps']
                }
            },
            
            'execution_plan': execution_plan,
            
            'impact_matrix': impact_matrix,
            
            'critical_paths': {
                'module_critical_path': data['dependencies']['critical_path'],
                'step_critical_path': [
                    s['step_id'] for s in data['impact']['critical_path_steps']
                ]
            },
            
            'recommendations': self._generate_recommendations(data)
        }
        
        # Write report
        output_path = Path(self.workspace_root) / output_file
        with open(output_path, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"   ✓ Report saved to: {output_file}")
        
        # Print summary
        self._print_summary(report)
    
    def _build_execution_plan(self, data: Dict) -> Dict:
        """Build integrated execution plan."""
        # Use impact analysis recommendations as base
        impact_plan = data['impact']['execution_recommendation']
        
        # Enhance with module dependency information
        enhanced_phases = []
        
        for phase in impact_plan['phases']:
            # Get items for this phase
            phase_items = []
            
            for step_id in phase['items']:
                # Find corresponding mapping and assessment
                mapping = next(
                    (m for m in data['modules']['mappings'] if m['step_id'] == step_id),
                    None
                )
                assessment = next(
                    (a for a in data['impact']['assessments'] if a['step_id'] == step_id),
                    None
                )
                
                if mapping and assessment:
                    phase_items.append({
                        'step_id': step_id,
                        'title': mapping['title'],
                        'primary_module': mapping['primary_module'],
                        'affected_count': assessment['affected_count'],
                        'dependency_count': assessment['dependency_count'],
                        'impact_score': assessment['impact_score'],
                        'confidence': mapping['confidence']
                    })
            
            enhanced_phases.append({
                'phase_number': phase['phase'],
                'phase_name': phase['name'],
                'rationale': phase['rationale'],
                'item_count': len(phase_items),
                'items': sorted(phase_items, 
                              key=lambda x: -x['impact_score'])[:50]  # Top 50
            })
        
        return {
            'total_phases': len(enhanced_phases),
            'phases': enhanced_phases
        }
    
    def _build_impact_matrix(self, data: Dict) -> Dict:
        """Build impact matrix showing module-to-step relationships."""
        # Group steps by primary module
        module_to_steps = {}
        
        for mapping in data['modules']['mappings']:
            module = mapping['primary_module']
            if module not in module_to_steps:
                module_to_steps[module] = []
            
            module_to_steps[module].append({
                'step_id': mapping['step_id'],
                'title': mapping['title'],
                'confidence': mapping['confidence']
            })
        
        # Find most impactful modules
        impact_scores = {}
        for module, steps in module_to_steps.items():
            impact_scores[module] = len(steps)
        
        return {
            'total_modules_affected': len(module_to_steps),
            'top_modules': [
                {
                    'module': module,
                    'step_count': score,
                    'steps': module_to_steps[module]
                }
                for module, score in sorted(
                    impact_scores.items(),
                    key=lambda x: x[1],
                    reverse=True
                )[:20]
            ]
        }
    
    def _generate_recommendations(self, data: Dict) -> List[Dict]:
        """Generate actionable recommendations."""
        recommendations = []
        
        # Recommendation 1: Start with foundational items
        foundational_count = data['impact']['metadata']['foundational_steps']
        recommendations.append({
            'priority': 'HIGH',
            'category': 'Execution Order',
            'recommendation': f'Begin with {foundational_count} foundational items',
            'rationale': 'These have minimal dependencies and can be executed immediately in parallel'
        })
        
        # Recommendation 2: Address high-impact items early
        high_impact_count = data['impact']['metadata']['high_impact_steps']
        recommendations.append({
            'priority': 'HIGH',
            'category': 'Risk Mitigation',
            'recommendation': f'Execute {high_impact_count} high-impact items in Phase 2',
            'rationale': 'Affects many modules; doing early minimizes cascading rework'
        })
        
        # Recommendation 3: Circular dependencies
        cycles = data['dependencies']['metadata']['cycles_detected']
        if cycles > 0:
            recommendations.append({
                'priority': 'MEDIUM',
                'category': 'Code Quality',
                'recommendation': f'Refactor {cycles} circular dependency groups',
                'rationale': 'Circular dependencies complicate maintenance and testing'
            })
        else:
            recommendations.append({
                'priority': 'LOW',
                'category': 'Code Quality',
                'recommendation': 'Maintain zero circular dependencies',
                'rationale': 'Current codebase has clean dependency structure'
            })
        
        # Recommendation 4: Low confidence mappings
        low_conf = sum(
            1 for m in data['modules']['mappings'] 
            if m['confidence'] < 0.4
        )
        if low_conf > 10:
            recommendations.append({
                'priority': 'MEDIUM',
                'category': 'Validation',
                'recommendation': f'Manually review {low_conf} low-confidence module mappings',
                'rationale': 'Automated mapping has low confidence; human validation recommended'
            })
        
        # Recommendation 5: Parallelization
        phase1_count = len(data['impact']['execution_recommendation']['phases'][0]['items'])
        recommendations.append({
            'priority': 'HIGH',
            'category': 'Performance',
            'recommendation': f'Execute {phase1_count} Phase 1 items in parallel',
            'rationale': f'No blocking dependencies; can use {min(phase1_count, 10)} parallel workers'
        })
        
        return recommendations
    
    def _print_summary(self, report: Dict) -> None:
        """Print comprehensive summary."""
        print("\n" + "="*70)
        print("PHASE 4: CROSS-REFERENCE RESOLUTION - COMPLETE")
        print("="*70)
        
        meta = report['metadata']
        summary = report['summary']
        
        print(f"\n📊 Overall Statistics:")
        print(f"  Roadmap Steps: {meta['total_steps']}")
        print(f"  Agda Modules: {meta['total_modules']}")
        print(f"  Dependencies: {meta['total_dependencies']}")
        
        print(f"\n🔗 Module Mappings:")
        mappings = summary['module_mappings']
        print(f"  High Confidence: {mappings['high_confidence']} ({mappings['high_confidence']/meta['total_steps']*100:.1f}%)")
        print(f"  Medium Confidence: {mappings['medium_confidence']} ({mappings['medium_confidence']/meta['total_steps']*100:.1f}%)")
        print(f"  Low Confidence: {mappings['low_confidence']} ({mappings['low_confidence']/meta['total_steps']*100:.1f}%)")
        
        print(f"\n🕸️  Dependency Structure:")
        deps = summary['dependency_structure']
        print(f"  Dependency Layers: {deps['layers']}")
        print(f"  Critical Path Length: {deps['critical_path_length']}")
        print(f"  Circular Dependencies: {deps['cycles_detected']}")
        print(f"  Avg Dependencies/Module: {deps['total_edges']/deps['total_modules']:.1f}")
        
        print(f"\n🎯 Impact Analysis:")
        impact = summary['impact_analysis']
        print(f"  High Impact Steps: {impact['high_impact_steps']}")
        print(f"  Foundational Steps: {impact['foundational_steps']}")
        print(f"  Critical Path Steps: {impact['critical_path_steps']}")
        
        print(f"\n📋 Execution Plan:")
        for phase in report['execution_plan']['phases']:
            print(f"  Phase {phase['phase_number']}: {phase['phase_name']}")
            print(f"    → {phase['item_count']} items")
            print(f"    → {phase['rationale']}")
        
        print(f"\n💡 Top Recommendations:")
        for i, rec in enumerate(report['recommendations'][:5], 1):
            print(f"  {i}. [{rec['priority']}] {rec['recommendation']}")
            print(f"     → {rec['rationale']}")
        
        print("\n" + "="*70)
        print("✓ Phase 4 Complete - Ready for Implementation")
        print("="*70)


def main():
    """Main entry point."""
    print("="*70)
    print("PHASE 4: CROSS-REFERENCE REPORT GENERATOR")
    print("="*70)
    
    reporter = CrossReferenceReporter()
    
    # Load all data
    data = reporter.load_all_data()
    
    # Generate integrated report
    reporter.generate_integrated_report(data)


if __name__ == '__main__':
    main()
