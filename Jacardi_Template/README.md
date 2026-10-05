# JACARDI JSON templates

Author: Mahima Ghosh

Reusable shapes for every JACARDI country pack (`Jacardi_Slovenia`, `Jacardi_Romania`, …).

| File | Role |
|------|------|
| `config.template.json` | Host run config only (country, years, diseases, `project_requirements`, model paths) |
| `jacardi_model.template.json` | Static slot — `ModelName: JacardiModel` + CSV slots (no numeric coeffs) |
| `jacardi_model_update.template.json` | Dynamic slot — `ModelName: JacardiModelUpdate` + same CSV slots |

**Rules**

- Numbers / probabilities / coefficients live in **CSVs**, not in these JSON files.
- Copy the three templates into a country folder, rename (drop `.template`), fill `REPLACE_*` and country CSV names.
- Later variables (employment, HLS, smoking, …) = add CSV slots to both model JSONs; do not invent a new host layout.
- Do **not** add an `education` block under `project_requirements` until the schema allows it (`additionalProperties: false` today). Education is enabled by CSV slots + schedule.

See also: Health-GPS plan `documentation/technical/plans/JACARDI-7-countries-implementation-plan.md`.
For any issues, please reach out to Mahima
