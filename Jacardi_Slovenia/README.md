# Jacardi_Slovenia

Author: Mahima Ghosh

Country pack for JACARDI Slovenia (ISO3 `SVN`). Horizon: **2025–2055**.

## Files

| File | Role |
| --- | --- |
| `config.json` | Host; `risk_factors` levels = ladder order |
| `jacardi_model.json` | Init — education lookup + init coef slots (employment…SBP) |
| `jacardi_model_update.json` | Update — education Part B + yearly coef slots (employment…SBP) |
| `education_*.csv` | Delivered education tables (lookup / draw22 / upgrades) |
| `Slovenia.DataFile.csv` | Minimal dataset stub |

Templates: [`../Jacardi_Template/`](../Jacardi_Template/).

## Ladder → where it lives

| Level | Variable | Init file | Update file |
| --- | --- | --- | --- |
| 0 | Age, Sex | UNDB / DemographicModule | ageing in DemographicModule |
| 1 | Education | `education_lookup.csv` | `education_draw_at_22_ssp2.csv` + `education_upgrade_transitions_ssp2.csv` |
| 2–6 | Employment … SBP | `*_coefs.csv` slots in `jacardi_model.json` | `*_update_coefs.csv` slots in `jacardi_model_update.json` |
| 7 | MI | `running.diseases: myocardial` + datastore RRs | DiseaseModule |

Education CSVs are on disk. Other coef CSVs are named in the JSON slots and arrive when partners deliver them.

## Factors mean (baseline_adjustments)

With `project_requirements.risk_factors.adjust_to_factors_mean: false`, JACARDI does not calibrate risk factors to population means — no `FactorsMean.*.csv` is needed. Omit `baseline_adjustments.file_names` entirely (do not use `"file_names": {}`; the config schema requires `factorsmean_male` / `factorsmean_female` if that object is present). HLM/FINCH packs still list those files because their loaders read them at init even when adjustment is off.

## SES noise (`ses_model`)

Omit `modelling.ses_model`. JACARDI does not use continuous `Person.ses` noise; socioeconomic pathways go through Education / Employment / … India/HLM packs that still need SES keep the block (or set `"enabled": true`). If a leftover block is present, set `"enabled": false` to ignore it.

## Not runnable yet

Engine does not yet register `JacardiModel` / `JacardiModelUpdate`. Fill `data.source` / `checksum` when the datastore zip is ready.
