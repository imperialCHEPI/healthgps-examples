# Jacardi_Slovenia

Author: Mahima Ghosh

Country pack for JACARDI Slovenia (ISO3 `SVN`). Horizon: **2025–2055**.

## Files

| File | Role |
| --- | --- |
| `config.json` | Host; `risk_factors` levels = ladder order |
| `jacardi_model.json` | Init — education lookup + employment static + later coef slots |
| `jacardi_model_update.json` | Update — education Part B + employment static Part B + later yearly slots |
| `education_*.csv` | Delivered education tables (lookup / draw19 / upgrades; algo 15 Oct 2026) |
| `employment_static_coefs.csv` | Multinomial SCORE coefs (16 Oct 2026) |
| `employment_4cat_recode.csv` | Map employment_id 1–6 → employment4_id 1–4 |
| `employment_static_check.csv` | Golden SCORE probs (tests only; not a runtime slot) |
| `EMPLOYMENT_STATIC_ALGORITHM.md` | Partner employment static rules |
| `Slovenia.DataFile.csv` | Minimal dataset stub |

Templates: [`../Jacardi_Template/`](../Jacardi_Template/).

## Ladder → where it lives

| Level | Variable | Init file | Update file |
| --- | --- | --- | --- |
| 0 | Age, Sex | UNDB / DemographicModule | ageing in DemographicModule |
| 1 | Education | `education_lookup.csv` | `education_draw_at_19_ssp2.csv` + `education_upgrade_transitions_ssp2.csv` |
| 2 | EmploymentDetailed (1–6) + Employment (1–4) | `employment_static_coefs.csv` + `employment_4cat_recode.csv` | Same CSVs (first draw at 19; temporary hold until yearly package) |
| 3–6 | HLS … SBP | `*_coefs.csv` slots in `jacardi_model.json` | `*_update_coefs.csv` slots in `jacardi_model_update.json` |
| 7 | MI | `running.diseases: myocardial` + datastore RRs | DiseaseModule |

Education and employment static CSVs are on disk. HLS…SBP coef CSVs are named in the JSON slots and arrive when partners deliver them.

## Factors mean (baseline_adjustments)

With `project_requirements.risk_factors.adjust_to_factors_mean: false`, JACARDI does not calibrate risk factors to population means — no `FactorsMean.*.csv` is needed. Omit `baseline_adjustments.file_names` entirely (do not use `"file_names": {}`; the config schema requires `factorsmean_male` / `factorsmean_female` if that object is present). HLM/FINCH packs still list those files because their loaders read them at init even when adjustment is off.

## SES noise (`ses_model`)

Omit `modelling.ses_model`. JACARDI does not use continuous `Person.ses` noise; socioeconomic pathways go through Education / Employment / … India/HLM packs that still need SES keep the block (or set `"enabled": true`). If a leftover block is present, set `"enabled": false` to ignore it.

## Not runnable yet

Engine does not yet register `JacardiModel` / `JacardiModelUpdate`. Fill `data.source` / `checksum` when the datastore zip is ready.
