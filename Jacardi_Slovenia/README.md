# Jacardi_Slovenia
Author: Mahima Ghosh

Country pack for JACARDI Slovenia (ISO3 `SVN`). Horizon: **2025–2055**.

## Files

| File | Role |
| --- | --- |
| `config.json` | Host; `risk_factors` levels = ladder order |
| `jacardi_model.json` | **Init** — `education_lookup.csv` + slots for employment…SBP coef CSVs |
| `jacardi_model_update.json` | **Update** — `education_draw_at_22_ssp2.csv` + `education_upgrade_transitions_ssp2.csv` only |
| `education_*.csv` | Delivered education tables |
| `Slovenia.DataFile.csv` | Minimal dataset stub |

Templates: [`../Jacardi_Template/`](../Jacardi_Template/).

## Ladder → where it lives

| Level | Variable | Init | Update |
| --- | --- | --- | --- |
| 0 | Age, Sex | UNDB / DemographicModule | ageing in DemographicModule |
| 1 | Education | `jacardi_model.json` → lookup | `jacardi_model_update.json` → draw22 + upgrades |
| 2–6 | Employment … SBP | coef CSV slots in `jacardi_model.json` (when files arrive) | none yet |
| 7 | MI | `running.diseases: myocardial` + datastore RRs | DiseaseModule |

## Not runnable yet

Engine does not yet register `JacardiModel` / `JacardiModelUpdate`. Fill `data.source` / `checksum` when the datastore zip is ready. Coef CSVs named in init slots are placeholders until partners deliver them.
