# Formalism Adapter Schema

Minimal adapter description for mapping a formalism to canonical universal
construction signatures.

## Top-Level Fields

- `formalism_id` (string, required)
- `version` (string, required)
- `constructions` (array, required)
- `metadata` (object, optional)

## Construction Entry

- `construction_id` (string, required)
- `kind` (string, required; e.g. `limit`, `colimit`, `adjunction`, `kanExtension`)
- `objects` (array of strings)
- `morphisms` (array of strings)
- `property_id` (string, required)
- `notes` (string, optional)
- `sources` (array of strings, optional)

## Example

```json
{
  "formalism_id": "example-formalism",
  "version": "1.0",
  "constructions": [
    {
      "construction_id": "Product",
      "kind": "limit",
      "objects": ["A", "B"],
      "morphisms": [],
      "property_id": "ProductProperty",
      "notes": "Minimal product signature",
      "sources": ["docs/architecture/example.md"]
    }
  ],
  "metadata": {
    "owner": "core"
  }
}
```
