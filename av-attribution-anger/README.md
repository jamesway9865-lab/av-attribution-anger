# Same error, different explanations: data and code

Data and analysis code for

> Wang, T., Sun, H., Ge, Y., & Qu, W. (in preparation). *Same error, different explanations: Attributions and anger toward automated and human-driven vehicles.* Manuscript submitted to *Accident Analysis & Prevention*.

Two online questionnaire studies with licensed drivers in China:

- **Study 1** (N = 1,202): drivers rated nine everyday driving errors (ten conditions) committed either by an automated vehicle (AV) or by a human-driven vehicle (HV; between participants) — the likelihood of candidate causes, their anger and their blame; afterwards they rated the target's driving safety and ability (current level and the level it should reach).
- **Study 2** (N = 723 of 800 randomised): AVs only; drivers received information on AVs' driving ability, information on AVs' rule compliance, or no information, and then rated five AV errors (anger, punishment, causes).

Every number, table and figure in the Results section can be regenerated from `data/source/` with the scripts in `code/`.

## Repository structure

```
data/
  source/        study1_source.csv, study2_source.csv   variables used in the paper (original names; platform IDs removed)
  processed/     study1_long.csv, study1_person.csv, study2_long.csv, study2_person.csv   analysis files
  raw/           not shared (see data/raw/README.md)
code/
  functions.R                   shared helpers (t tests, within/between decomposition, mixed models, mediation, rounding)
  00_make_source_data.R         authors only: raw questionnaire exports -> data/source/
  01_prepare_data.R             data/source/ -> data/processed/
  02_results.R                  all Results numbers, Tables 1-5, Supplementary Tables S1-S6 -> output/tables/
  03_figures.R                  Figures 1-5 -> output/figures/
  04_robustness_blame_wording.R robustness check for the Study 1 blame item (not reported in the manuscript)
  05_manuscript_tables.R        Tables 1-5 and S1-S6 formatted as printed -> output/manuscript_tables/
  run_all.R                     runs all steps in order and writes output/run_log.txt
output/
  tables/              one CSV per result table (results_tables.xlsx as well if openxlsx is installed), session_info.txt
  manuscript_tables/   Tables 1-5 and S1-S6 as printed in the manuscript (CSV and Markdown)
  figures/             Fig1-Fig5 as 300-dpi PNG and vector PDF
docs/
  codebook.md          variables, scales, error conditions, item classes, order of measurement
  manuscript_map.md    which file each table, figure and paragraph of the Results comes from
```

## How to reproduce

R (≥ 4.1) with the packages lme4, lmerTest and clubSandwich (installed automatically if missing; openxlsx is optional).
From the repository root:

```
Rscript code/run_all.R
```

or, in RStudio, set the working directory to the repository root and run `source("code/run_all.R")`. The whole run takes
several minutes (mostly for the cluster-robust standard errors). Bootstrap analyses set the seed 20261118 before each
analysis, so all results are exactly reproducible. R and package versions used for the reported results are listed in
`output/tables/session_info.txt`.

## Methods in brief

- **Mixed models.** Error-level responses were analysed with linear mixed models with a random intercept for participants and fixed effects for errors (REML; lme4). Attribution indices were centred on their error mean and split into within-person and between-person components. Tests use Satterthwaite degrees of freedom (lmerTest); cluster-robust standard errors are CR2, clustered by participant (clubSandwich).
- **Decomposition.** Person-level parallel mediation of the AV–HV difference in anger through mean trait, momentary and situational attributions, with attribution–anger paths allowed to differ by vehicle type; counterfactual indirect and direct effects; 95% percentile bootstrap CIs (2,000 resamples, stratified by condition).
- **Variation across errors.** Between-error heterogeneity (τ) of the AV–HV difference in anger (Cohen's *d*), estimated by subtracting the mean bootstrap sampling variance from the observed variance; 95% prediction interval.
- **Study 2.** Information effects in mixed models with control as reference; Holm correction for the eight primary tests; Benjamini–Hochberg FDR for error-specific comparisons.

## Data notes

- Study 2: of the 800 randomised participants, 77 whose rating of the targeted perception decreased after the information were excluded (ability condition 33, rule-compliance condition 44). The data contain the 723 retained participants; this exclusion cannot be reproduced from the shared files.
- In the AV version of Study 1, the blame item for three conditions (slow vehicle ahead, sudden braking, stopping behind an obstacle) referred to "the driver of the vehicle ahead"; blame is therefore a secondary outcome (see `code/04_robustness_blame_wording.R`).
- The questionnaires were part of a larger project; measures not reported in the paper are not included.

## License and citation

License: [to be added by the authors, e.g. CC BY 4.0 for the data and MIT for the code].
Please cite the article (see `CITATION.cff`).

Contact: Weina Qu, Institute of Psychology, Chinese Academy of Sciences (quwn@psych.ac.cn).
