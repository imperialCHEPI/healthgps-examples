# JACARDI JSON templates

Author: Mahima Ghosh

Reusable shapes for every JACARDI country pack (`Jacardi_Slovenia`, `Jacardi_Romania`, …).

| File | Role |
| --- | --- |
| `config.template.json` | Host run config (country, years, `risk_factors` levels, model paths) |
| `jacardi_model.template.json` | Static / init slot — init CSV names only |
| `jacardi_model_update.template.json` | Dynamic / update slot — yearly CSV names only |

## Static vs dynamic

Do not mirror the same files across slots.

| Slot | Education | Other ladder vars |
| --- | --- | --- |
| `JacardiModel` | `lookup` (Part A ages 22+) | employment, HLS, smoking, PA, BMI, meds, SBP coef CSVs |
| `JacardiModelUpdate` | `draw_at_22` + `upgrade_transitions` (Part B) | yearly update CSVs only when needed later |

Age/sex come from UNDB (DemographicModule), not these JSONs. MI is a disease (`running.diseases`), not a Jacardi coef slot.

## Rules

- Numbers / probabilities / coefficients live in **CSVs**, not in these JSON files.
- No `schedule.csv` — order of inclusion is `modelling.risk_factors[].level` in `config.json`. Drop the CSV filename into the matching slot when partners deliver it.
- Copy the three templates into a country folder, rename (drop `.template`), fill `REPLACE_*` and country CSV names.
- Do **not** add an `education` block under `project_requirements` until the schema allows it (`additionalProperties: false` today).

See also: Health-GPS plan `documentation/technical/plans/JACARDI-7-countries-implementation-plan.md`.
For any issues, please reach out to Mahima.
