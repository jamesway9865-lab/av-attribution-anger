# Where each result in the manuscript comes from

All analyses are run in R. All files are in `output/tables/` and are written by `code/02_results.R` unless stated
otherwise. Formatted versions of Tables 1–5 and Supplementary Tables S1–S6 (`TableS1.csv` … `TableS6.csv`) are written by
`code/05_manuscript_tables.R` to `output/manuscript_tables/`. Figures are written by `code/03_figures.R` to `output/figures/`.

## Tables

| Manuscript | Result files |
|---|---|
| Table 1 Sample characteristics | `T1_samples.csv`, `T1b_balance.csv` |
| Table 2 Study 1 group differences | `S1_traits_expectations.csv`, `S1_attributions_overall.csv`, `S1_overall_vehicle_effect.csv` (anger, blame) |
| Table 3 Attribution–anger slopes | `S1_multilevel_slopes.csv` (model A, outcome = anger), `S2_multilevel_slopes.csv` (outcome = anger); ICC: `S1_multilevel_icc.csv`, `S2_multilevel_slopes.csv` |
| Table 4 Study 1 decomposition | `S1_decomposition_person_level.csv`, `S1_direct_effect_reduction.csv` |
| Table 5 Study 2 information effects | `S2_group_effects.csv`; decomposition: `S2_decomposition_person_level.csv` |

## Text

| Section | Result files |
|---|---|
| 3.1 Sample and randomisation check | `T1_samples.csv`, `T1b_balance.csv` |
| 3.2 Perceived characteristics and expectations | `S1_traits_expectations.csv` |
| 3.3 Attributions (RQ1) | `S1_attributions_overall.csv`; by error: `S1_by_event_attributions_anger_blame.csv` |
| 3.4 Anger toward AVs and human drivers | `S1_overall_vehicle_effect.csv`, `S1_by_event_attributions_anger_blame.csv`, `S1_anger_heterogeneity.csv`, `S1_trait_anger_difference_correlation.csv` |
| 3.5 Attributions and anger (RQ2) | `S1_multilevel_slopes.csv` (model A; outcome = anger, and outcome = blame), `S1_multilevel_icc.csv` |
| 3.6 Decomposition (RQ3) | `S1_decomposition_person_level.csv` (spec = attributions); by error: `S1_decomposition_by_event.csv` |
| 3.7 Trait content and expectation gaps | `S1_decomposition_person_level.csv` (spec = attributions (trait split); + gaps), `S1_multilevel_slopes.csv` (model B), `S1_gap_moderation.csv`, `S1_direct_effect_reduction.csv` |
| 4.1 Sample and manipulation check | `T1_samples.csv`, `T1b_balance.csv`, `S2_manipulation_check.csv`; adjusted effects: `S2_group_effects_adjusted.csv` |
| 4.2 Information and attributions (RQ4) | `S2_group_effects.csv`; by error: `S2_by_event.csv` (outcome = trait) |
| 4.3 Information, anger and punishment | `S2_group_effects.csv`; by error: `S2_by_event.csv` (outcome = anger, punishment; FDR in `p_fdr`) |
| 4.4 Attributions as a pathway | `S2_multilevel_slopes.csv`, `S2_decomposition_person_level.csv` |
| Robustness check, blame wording (not yet in the manuscript) | `S1_blame_wording_robustness_overall.csv`, `_slopes.csv`, `_anger_blame_corr.csv` (`code/04_robustness_blame_wording.R`) |

## Figures

| Figure | Data | File |
|---|---|---|
| Fig. 1 Path model, Study 1 | `S1_decomposition_person_level.csv`, `S1_direct_effect_reduction.csv` | `Fig1_path_model_study1.pdf/.png` |
| Fig. 2 Trait attribution and anger by error, Study 1 | `S1_by_event_attributions_anger_blame.csv`, `S1_anger_heterogeneity.csv` | `Fig2_by_event_trait_anger_study1.pdf/.png` |
| Fig. 3 Slopes by vehicle type, Study 1 | `S1_multilevel_slopes.csv` (model A, anger) | `Fig3_slopes_by_vehicle_study1.pdf/.png` |
| Fig. 4 Decomposition by error, Study 1 | `S1_decomposition_by_event.csv` | `Fig4_decomposition_by_event_study1.pdf/.png` |
| Fig. 5 Information effects, Study 2 | `S2_group_effects.csv`, `S2_by_event.csv` | `Fig5_information_effects_study2.pdf/.png` |

## Supplementary tables

| Supplementary table | Result file |
|---|---|
| S1 AV–HV differences by error (Study 1) | `S1_by_event_attributions_anger_blame.csv` |
| S2 Decomposition by error (Study 1; Fig. 4) | `S1_decomposition_by_event.csv` |
| S3 Study 2 effects adjusted for age, driving experience, gender | `S2_group_effects_adjusted.csv` |
| S4 Information effects by error (Study 2) | `S2_by_event.csv` |
| S5 Slopes for blame and for competence- vs. character-related attributions (Study 1) | `S1_multilevel_slopes.csv` (outcome = blame; model B) |
| S6 Manipulation check (Study 2) | `S2_manipulation_check.csv` |
| S7 Attribution items and their classification | `docs/codebook.md` and `code/01_prepare_data.R` (item wording: questionnaire) |

## What cannot be reproduced from the shared data

- The exclusion of 77 of the 800 randomised Study 2 participants (Section 2.3.1): the source data contain only the 723 retained participants.
- Screening of Study 1 respondents without a licence or driving experience, which happened inside the online questionnaire.
