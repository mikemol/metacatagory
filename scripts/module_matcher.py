#!/usr/bin/env python3
"""
Module Matcher - Phase 4 Cross-Reference Resolution
Maps RoadmapStep items to relevant Agda modules based on:
- Keywords and content analysis
- Module structure and naming patterns
- Existing dependencies
"""

import json
import os
import re
from pathlib import Path
from typing import Dict, List, Set, Tuple
from dataclasses import dataclass, field
from collections import defaultdict

@dataclass
class AgdaModule:
    """Represents an Agda module with metadata."""
    path: str
    name: str
    imports: List[str] = field(default_factory=list)
    keywords: Set[str] = field(default_factory=set)
    category: str = ""
    
    @property
    def qualified_name(self) -> str:
        """Get the fully qualified module name."""
        # Convert path to qualified name: src/agda/Core/Phase.agda -> Core.Phase
        rel_path = self.path.replace('src/agda/', '')
        module_path = rel_path.replace('.agda', '').replace('/', '.')
        return module_path

@dataclass
class ModuleMapping:
    """Maps a RoadmapStep to relevant Agda modules."""
    step_id: str
    title: str
    primary_module: str
    related_modules: List[str] = field(default_factory=list)
    confidence: float = 0.0
    rationale: str = ""

class ModuleMatcher:
    """Matches roadmap steps to Agda modules."""
    
    def __init__(self, workspace_root: str = str(Path(__file__).resolve().parent.parent)):
        self.workspace_root = workspace_root
        self.agda_modules: Dict[str, AgdaModule] = {}
        self.category_keywords = self._define_category_keywords()
        
    def _define_category_keywords(self) -> Dict[str, Set[str]]:
        """Define keywords for different module categories."""
        return {
            'core': {'category', 'functor', 'morphism', 'object', 'witness', 
                    'proof', 'universal', 'yoneda', 'fibration'},
            'algebra': {'group', 'ring', 'field', 'module', 'vector', 'space',
                       'algebra', 'polynomial', 'galois', 'abelian'},
            'plan': {'roadmap', 'pandoc', 'document', 'synthesis', 'chip',
                    'framework', 'metadata', 'utility'},
            'chapter': {'level', 'index', 'definition', 'theorem', 'categorical'},
            'examples': {'example', 'demo', 'test', 'witness', 'checklist'},
            'algorithms': {'algorithm', 'complexity', 'correctness', 'evidence',
                         'inductive', 'classification', 'registry'},
            'markdown': {'markdown', 'normalization', 'export', 'proof'},
            'tests': {'test', 'validation', 'checklist', 'status'},
        }
    
    def scan_agda_modules(self) -> None:
        """Scan all Agda modules and extract metadata."""
        print("🔍 Scanning Agda modules...")
        
        agda_dir = Path(self.workspace_root) / "src" / "agda"
        agda_files = list(agda_dir.rglob("*.agda"))
        
        for agda_file in agda_files:
            module = self._analyze_module(agda_file)
            self.agda_modules[module.qualified_name] = module
        
        print(f"   ✓ Scanned {len(self.agda_modules)} modules")
    
    def _analyze_module(self, file_path: Path) -> AgdaModule:
        """Analyze a single Agda module."""
        rel_path = str(file_path.relative_to(self.workspace_root))
        
        # Extract module name from file
        module_name = file_path.stem
        
        # Read file to extract imports and keywords
        try:
            content = file_path.read_text(encoding='utf-8')
            imports = self._extract_imports(content)
            keywords = self._extract_keywords(content)
        except Exception:
            imports = []
            keywords = set()
        
        # Determine category based on path
        category = self._determine_category(rel_path)
        
        return AgdaModule(
            path=rel_path,
            name=module_name,
            imports=imports,
            keywords=keywords,
            category=category
        )
    
    def _extract_imports(self, content: str) -> List[str]:
        """Extract import statements from Agda file."""
        imports = []
        import_pattern = re.compile(r'^\s*(?:open )?import\s+([\w.]+)', re.MULTILINE)
        
        for match in import_pattern.finditer(content):
            module_name = match.group(1)
            # Filter out Agda.Builtin imports
            if not module_name.startswith('Agda.Builtin') and \
               not module_name.startswith('Agda.Primitive'):
                imports.append(module_name)
        
        return imports
    
    def _extract_keywords(self, content: str) -> Set[str]:
        """Extract keywords from module content."""
        keywords = set()
        
        # Extract from comments
        comment_pattern = re.compile(r'--\s*(.+)$', re.MULTILINE)
        for match in comment_pattern.finditer(content):
            words = match.group(1).lower().split()
            keywords.update(w.strip('.,;:()[]{}') for w in words if len(w) > 3)
        
        # Extract from record/data type definitions
        type_pattern = re.compile(r'(?:record|data)\s+(\w+)', re.MULTILINE)
        for match in type_pattern.finditer(content):
            keywords.add(match.group(1).lower())
        
        return keywords
    
    def _determine_category(self, file_path: str) -> str:
        """Determine module category from file path."""
        path_lower = file_path.lower()
        
        if '/core/' in path_lower:
            return 'core'
        elif '/algebra/' in path_lower:
            return 'algebra'
        elif '/plan/cim/' in path_lower:
            return 'plan'
        elif '/chapter' in path_lower:
            return 'chapter'
        elif '/examples/' in path_lower:
            return 'examples'
        elif '/tests/' in path_lower:
            return 'tests'
        elif '/markdown/' in path_lower:
            return 'markdown'
        else:
            return 'other'
    
    def match_roadmap_to_modules(
        self,
        metadata_file: str = "build/ingested_metadata.json"
    ) -> Dict[str, ModuleMapping]:
        """Match each roadmap step to relevant modules.

        Fallback: if metadata_file is missing, use data/planning_index.json
        (generated from PlanningKernel) to keep prioritization running.
        """
        print("\n🔗 Matching roadmap steps to modules...")
        
        files = {}
        metadata_path = Path(self.workspace_root) / metadata_file
        if metadata_path.exists():
            with open(metadata_path, 'r') as f:
                data = json.load(f)
            files = data.get('files', {})
        else:
            fallback_path = Path(self.workspace_root) / "data" / "planning_index.json"
            try:
                with open(fallback_path, 'r') as f:
                    index_items = json.load(f)
            except json.JSONDecodeError:
                text = fallback_path.read_text()
                text = text.replace('\\', '\\\\')
                index_items = json.loads(text)
            for item in index_items:
                files[item['id']] = {
                    'title': item.get('title', ''),
                    'summary': item.get('category', ''),
                    'keywords': item.get('tags', []),
                    'files': item.get('files', []),
                }
            print(f"   ⚠️  {metadata_file} not found; using planning_index.json fallback with {len(files)} items.")
        
        mappings = {}
        
        for step_id, item_data in files.items():
            # Add step_id to item data
            item = {'id': step_id, **item_data}
            # Prefer explicit file mapping when present (e.g., planning_index.json entries)
            file_modules = [f for f in item_data.get('files', []) if f.startswith("src/agda/")]
            if file_modules:
                primary_module = file_modules[0].replace("src/agda/", "").replace(".agda", "").replace("/", ".")
                mapping = ModuleMapping(
                    step_id=step_id,
                    title=item.get('title', ''),
                    primary_module=primary_module,
                    related_modules=[],
                    confidence=1.0,
                    rationale="Derived from explicit file path"
                )
            else:
                mapping = self._match_single_step(item)
            mappings[step_id] = mapping
        
        print(f"   ✓ Matched {len(mappings)} roadmap steps")
        
        return mappings
    
    def _match_single_step(self, item: Dict) -> ModuleMapping:
        """Match a single roadmap step to modules."""
        step_id = item['id']
        title = item['title']
        summary = item.get('summary', '')
        keywords = set(item.get('keywords', []))
        
        # Extract category from step_id (e.g., GP700 -> analysis phase)
        match = re.search(r'\d+', step_id)
        gp_number = int(match.group()) if match else 0
        step_category = self._categorize_gp_number(gp_number)
        
        # Score all modules
        module_scores = []
        for qualified_name, module in self.agda_modules.items():
            score = self._calculate_match_score(
                step_category, keywords, summary, module
            )
            if score > 0:
                module_scores.append((qualified_name, score))
        
        # Sort by score
        module_scores.sort(key=lambda x: x[1], reverse=True)
        
        # Select primary and related modules
        if module_scores:
            primary_module = module_scores[0][0]
            confidence = module_scores[0][1]
            related_modules = [m for m, s in module_scores[1:6] if s > 0.3]
            
            rationale = self._generate_rationale(
                step_category, keywords, module_scores[0]
            )
        else:
            # Default to Plan.CIM.IngestedRoadmaps
            primary_module = "Plan.CIM.IngestedRoadmaps"
            confidence = 0.1
            related_modules = []
            rationale = "No strong matches found; using default module"
        
        return ModuleMapping(
            step_id=step_id,
            title=title,
            primary_module=primary_module,
            related_modules=related_modules,
            confidence=confidence,
            rationale=rationale
        )
    
    def _categorize_gp_number(self, gp_number: int) -> str:
        """Categorize GP number into phase."""
        if gp_number < 100:
            return 'foundational'
        elif gp_number < 200:
            return 'structural'
        elif gp_number < 300:
            return 'geometric'
        elif gp_number < 400:
            return 'topological'
        elif gp_number < 500:
            return 'homological'
        elif gp_number < 600:
            return 'polytope'
        elif gp_number < 700:
            return 'coherence'
        elif gp_number < 800:
            return 'analysis'
        else:
            return 'unified'
    
    def _calculate_match_score(
        self,
        step_category: str,
        keywords: Set[str],
        summary: str,
        module: AgdaModule
    ) -> float:
        """Calculate match score between step and module."""
        score = 0.0
        
        # Category matching (weight: 0.3)
        category_map = {
            'foundational': ['core', 'chapter'],
            'structural': ['core', 'algebra'],
            'geometric': ['core', 'algebra'],
            'topological': ['core', 'algebra'],
            'homological': ['core', 'algebra'],
            'polytope': ['plan', 'examples'],
            'coherence': ['core'],
            'analysis': ['core', 'tests'],
            'unified': ['plan', 'core'],
        }
        
        if module.category in category_map.get(step_category, []):
            score += 0.3
        
        # Keyword matching (weight: 0.4)
        keyword_overlap = keywords.intersection(module.keywords)
        if keywords:
            keyword_score = len(keyword_overlap) / len(keywords)
            score += 0.4 * keyword_score
        
        # Content analysis (weight: 0.3)
        summary_lower = summary.lower()
        module_name_lower = module.name.lower()
        
        if module_name_lower in summary_lower:
            score += 0.2
        
        # Check for specific term matches
        for category, terms in self.category_keywords.items():
            if module.category == category:
                matching_terms = sum(1 for term in terms if term in summary_lower)
                if matching_terms > 0:
                    score += 0.1 * min(matching_terms / len(terms), 1.0)
        
        return min(score, 1.0)
    
    def _generate_rationale(
        self,
        step_category: str,
        keywords: Set[str],
        top_match: Tuple[str, float]
    ) -> str:
        """Generate human-readable rationale for the match."""
        module_name, score = top_match
        
        parts = [
            f"Category: {step_category}",
            f"Matched to {module_name}",
            f"Confidence: {score:.2f}"
        ]
        
        if keywords:
            parts.append(f"Keywords: {', '.join(list(keywords)[:5])}")
        
        return " | ".join(parts)
    
    def generate_report(
        self,
        mappings: Dict[str, ModuleMapping],
        output_file: str = "data/module_mappings.json"
    ) -> None:
        """Generate report of module mappings."""
        print(f"\n📄 Generating module mapping report...")
        
        # Convert to serializable format
        report = {
            'metadata': {
                'total_steps': len(mappings),
                'total_modules': len(self.agda_modules),
                'timestamp': self._get_timestamp()
            },
            'mappings': [
                {
                    'step_id': m.step_id,
                    'title': m.title,
                    'primary_module': m.primary_module,
                    'related_modules': m.related_modules,
                    'confidence': round(m.confidence, 3),
                    'rationale': m.rationale
                }
                for m in mappings.values()
            ],
            'module_index': {
                qualified_name: {
                    'path': mod.path,
                    'category': mod.category,
                    'imports': mod.imports,
                    'keywords': list(mod.keywords)[:20]
                }
                for qualified_name, mod in self.agda_modules.items()
            }
        }
        
        # Write report
        output_path = Path(self.workspace_root) / output_file
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(output_path, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"   ✓ Report saved to: {output_file}")
        
        # Print summary
        self._print_summary(mappings)
    
    def _get_timestamp(self) -> str:
        """Get current timestamp."""
        from datetime import datetime
        return datetime.now().isoformat()
    
    def _print_summary(self, mappings: Dict[str, ModuleMapping]) -> None:
        """Print summary statistics."""
        print("\n" + "="*70)
        print("MODULE MAPPING SUMMARY")
        print("="*70)
        
        # Confidence distribution
        high_conf = sum(1 for m in mappings.values() if m.confidence >= 0.7)
        med_conf = sum(1 for m in mappings.values() if 0.4 <= m.confidence < 0.7)
        low_conf = sum(1 for m in mappings.values() if m.confidence < 0.4)
        
        print(f"\nConfidence Distribution:")
        print(f"  High (≥0.7):  {high_conf} items ({high_conf/len(mappings)*100:.1f}%)")
        print(f"  Med (0.4-0.7): {med_conf} items ({med_conf/len(mappings)*100:.1f}%)")
        print(f"  Low (<0.4):    {low_conf} items ({low_conf/len(mappings)*100:.1f}%)")
        
        # Most common target modules
        module_counts = defaultdict(int)
        for m in mappings.values():
            module_counts[m.primary_module] += 1
        
        print(f"\nTop Target Modules:")
        for module, count in sorted(module_counts.items(), 
                                    key=lambda x: x[1], 
                                    reverse=True)[:10]:
            print(f"  {module}: {count} items")
        
        print("\n" + "="*70)

def main():
    """Main entry point."""
    print("="*70)
    print("PHASE 4: MODULE MATCHER")
    print("="*70)
    
    matcher = ModuleMatcher()
    
    # Scan all Agda modules
    matcher.scan_agda_modules()
    
    # Match roadmap steps to modules
    mappings = matcher.match_roadmap_to_modules()
    
    # Generate report
    matcher.generate_report(mappings)
    
    print("\n✓ Module matching complete!")

if __name__ == '__main__':
    main()
