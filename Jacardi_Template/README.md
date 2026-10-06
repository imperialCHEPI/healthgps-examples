# JACARDI JSON templates

Author: Mahima Ghosh

Reusable shapes for every JACARDI country pack (`Jacardi_Slovenia`, `Jacardi_Romania`, …).

| File | Role |
| --- | --- |
| `config.template.json` | Host run config (country, years, `risk_factors` levels, model paths) |
| `jacardi_model.template.json` | Static / init — init CSV slots for the full ladder |
| `jacardi_model_update.template.json` | Dynamic / update — yearly CSV slots for the full ladder |

## Static vs dynamic

Same variables appear in both files; different CSVs (init vs yearly). Do not put init lookup in the update file, or update tables in the init file.

| Slot | Education | Employment … SBP |
| --- | --- | --- |
| `JacardiModel` | `lookup` (Part A) | `*_coefs.csv` (init) |
| `JacardiModelUpdate` | `draw_at_22` + `upgrade_transitions` (Part B) | `*_update_coefs.csv` (yearly) |

Age/sex = UNDB / DemographicModule. MI = `running.diseases`, not a Jacardi coef slot.

Template files use `REPLACE_*` filenames until you copy them into a country pack and fill real names. Slovenia already has education CSV names filled; other coef filenames are reserved slots until partners deliver the files.

## Rules

- Numbers / probabilities / coefficients live in **CSVs**, not in these JSON files.
- No `schedule.csv` — order of inclusion is `modelling.risk_factors[].level` in `config.json`.
- Copy the three templates into a country folder, rename (drop `.template`), fill `REPLACE_*`.
- Do **not** add an `education` block under `project_requirements` until the schema allows it.
- Omit `modelling.ses_model` (optional in schema; unused for JACARDI).

See also: Health-GPS plan `documentation/technical/plans/JACARDI-7-countries-implementation-plan.md`.
For any issues, please reach out to Mahima.
