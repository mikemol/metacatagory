from pathlib import Path

from scripts.formalism_template import render_template


def test_render_template_includes_module_and_ids(tmp_path):
    adapter_path = tmp_path / "formalism_adapter.json"
    adapter_path.write_text(
        """
{
  "formalism_id": "example-formalism",
  "version": "1.0",
  "constructions": [
    {
      "construction_id": "Product",
      "kind": "limit",
      "objects": ["A", "B"],
      "morphisms": [],
      "property_id": "ProductProperty"
    }
  ]
}
        """.strip()
    )

    rendered = render_template(adapter_path)
    assert "module Plan.CIM.FormalismTemplates where" in rendered
    assert "ProductConstruction" in rendered
    assert "M.mkId \"Product\"" in rendered
