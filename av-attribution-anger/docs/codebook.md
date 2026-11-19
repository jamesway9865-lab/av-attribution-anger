# Codebook

This file describes the source data (`data/source/`) and the four processed data files (`data/processed/`).
The processed files are created from the source data by `code/01_prepare_data.R`.

## Common variables (processed data)

| Variable | Meaning | Range |
|---|---|---|
| `trait_comp` | Competence-related trait attribution: the error reflects a stable lack of ability of the actor (e.g., poor driving skill, unable to handle the situation) | 1–11 = likelihood of the cause from 0% to 100% in 10% steps |
| `trait_char` | Character-related trait attribution: the error reflects a stable character or driving style of the actor (e.g., habitually aggressive, habitually disregards traffic rules, selfish) | 1–11 |
| `trait` | Trait (internal-stable) attribution: mean of the competence- and character-related causes available for the error | 1–11 |
| `momentary` | Momentary internal attribution: a temporary internal cause (e.g., distraction, a temporary malfunction) | 1–11 |
| `situational` | Situational attribution: a cause outside the actor (e.g., traffic conditions, an oncoming vehicle) | 1–11 |
| `anger` | Anger ("How angry would you be?" 1 = not angry at all, 7 = very angry) | 1–7 |
| `blame` | Study 1: blame ("How would you blame ...?" 1 = no need to blame at all, 7 = should be blamed very severely) | 1–7 |
| `punishment` | Study 2: "Does the AV need to pay a price for what it did?" (1 = not necessary at all, 7 = very necessary) | 1–7 |

- When an error had no candidate cause of a given class, the variable is missing by design.
- The class of every candidate cause is defined in `code/01_prepare_data.R` (dictionaries `COMP`, `CHAR`, `MOM`, `SIT` for Study 1).

## study1_long.csv (Study 1; one row per participant × error condition; 1,202 × 10)

| Variable | Meaning |
|---|---|
| `pid` | Participant number (`id` in the source data) |
| `vehicle`, `AV` | Vehicle type, randomised between participants: HV = human-driven vehicle (n = 603), AV = automated vehicle (n = 599; `AV` = 1) |
| `event_code`, `event_no`, `event` | Error condition (table below) |

| event_code | No. | Error condition | Attribution classes measured |
|---|---|---|---|
| CA01 | 1 | Slow vehicle ahead | character, momentary, situational |
| CA02 | 2 | Sudden braking ahead | competence, character, momentary, situational |
| CA03 | 3 | Stopping behind an obstacle | competence, situational |
| CA041 | 4a | Rear-end collision after braking, simple road | competence, momentary, situational |
| CA042 | 4b | Rear-end collision after braking, complex road | competence, momentary, situational |
| CS01 | 5 | High beams at night | competence, momentary, situational |
| CS02 | 6 | Accelerating while being overtaken | character, momentary, situational |
| CS03 | 7 | Cutting into a queue | character, momentary, situational |
| WA01 | 8 | Left turn without yielding | character, momentary, situational |
| WA02 | 9 | Taking a parking space | character, momentary, situational |

**Order of ratings within each condition:** causes (attributions), then anger, then blame.
**Note on the blame item:** in the AV version, the blame item for CA01–CA03 still referred to "the driver of the vehicle ahead"; the other conditions used neutral wording or named the AV (see `code/04_robustness_blame_wording.R`).

## study1_person.csv (Study 1; one row per participant)

| Variable | Meaning |
|---|---|
| `female` | 1 = woman (source `gender` = 2) |
| `age` | Age in years |
| `driving_years` | Years of driving experience |
| `mileage_10k_km` | Total driving mileage (10,000 km) |
| `drivertype_raw`, `education_raw` | Original codes, not recoded |
| `safety_current` / `safety_expected` | Overall driving safety of the target (AVs or human drivers), current level / level it should reach, 0–100 |
| `ability_current` / `ability_expected` | Mean of three driving-ability items (recognising other vehicles' intentions; handling unexpected events; coping with complex environments), current / should reach, 1–7 |
| `safety_gap`, `ability_gap` | Expectation gap = should-reach rating − current rating |
| `mean_*` | Participant mean across the ten conditions (over the conditions in which the class was measured) |

**Order of measurement:** perceived safety and ability were rated after all error scenarios.

## study2_long.csv (Study 2; one row per participant × error; 723 × 5; AVs only)

| Variable | Meaning |
|---|---|
| `pid` | Participant number (row order of the source data) |
| `group`, `group_code` | 0 = control (no information, n = 200), 1 = ability information (n = 267), 2 = rule-compliance information (n = 256) |
| `event_code` | E1 stopping behind an obstacle; E2 stopping in a right-turn lane; E3 sudden deceleration ahead; E4 accelerating while being overtaken; E5 cutting in at close range |
| `trait_comp` | source variable `NSC` |
| `trait_char` | mean of source variables `NSS` and `NSB`; errors E4 and E5 only |
| `trait` | mean of the available items among `NSC`, `NSS`, `NSB` |
| `momentary` | source variable `NUC` |
| `situational` | source variable `W` |

**Order of ratings within each error:** anger, punishment, then causes (attributions).

## study2_person.csv (Study 2; one row per participant)

| Variable | Meaning |
|---|---|
| `ability_pre` / `ability_post` | Perceived AV driving ability before / after the information, mean of three items, 1–7 (source `C1` / `C2`) |
| `normativity_pre` / `normativity_post` | Perceived AV rule compliance before / after the information: mean of "complies with traffic rules" and "values other people's lives" (source `B1` / `B2` = mean of items `B11`/`B31` and `B12`/`B32`; item `B21`/`B22` is not included) |
| `av_knowledge` | Familiarity with AVs (original code) |
| `driving_mileage_raw` | Driving mileage (original units) |

- **Control group post-test:** the control group received no information and has no post-test (set to missing).
- **Excluded participants:** 800 drivers were randomised (control 200, ability 300, rule compliance 300). Participants in the information conditions whose rating of the targeted perception decreased after the information were excluded (ability 33, rule compliance 44; total 77). The source data contain only the 723 retained participants.

## Source data (data/source/)

`study1_source.csv` and `study2_source.csv` contain only the variables used in this paper, with original variable names
(`code/00_make_source_data.R`). Platform respondent IDs were removed. Variable name patterns:

- Study 1: `BEHAVaggressive<code>_a1` … `_a5` = candidate causes 1–5 of error `<code>` (coded 1–11); `_angry` = anger; `_blame` = blame.
  `TRAIT_CA1_d` / `_p` = overall safety, current / should reach; `TRAIT_CA4`–`CA6` = the three driving-ability items. `HVorAV`: 1 = HV, 2 = AV.
- Study 2: `condition` (0/1/2 as above); `C11`–`C31` = ability items before the information, `C1` = their mean; `C12`–`C32`, `C2` = after;
  `B11`–`B31`, `B1` and `B12`–`B32`, `B2` = rule-compliance items and score before / after; `E<k>emo` = anger, `E<k>beh` = punishment,
  `E<k>NSC` / `NSS` / `NSB` / `NUC` / `W` = causes for error `E<k>` (coded 1–11).

## Not included

The original questionnaires also contained measures from a larger project that are not reported in this paper (yielding
intentions, moral dilemmas, further trait ratings, a driving-simulator study). These are not part of this repository.
