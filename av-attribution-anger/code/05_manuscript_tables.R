# =============================================================================
# Step 5: format the manuscript tables (Tables 1-5) and Supplementary Tables S1-S6 from the result files.
#
# Input : output/tables/*.csv (from 02_results.R), data/processed/study1_person.csv
# Output: output/manuscript_tables/Table1.csv ... Table5.csv, Table4_note.csv, TableS1.csv ... TableS6.csv,
#         and manuscript_tables.md (all tables in one Markdown file)
# Numbers are rounded once, from full precision, half away from zero (2 decimals; slopes 3 decimals);
# a confidence bound that would round to zero keeps one or two more decimals (e.g. 0.004, -0.0003).
# Run   : Rscript code/05_manuscript_tables.R
# =============================================================================
if (!exists("ROOT")) {
  a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  ROOT <- if (nzchar(Sys.getenv("REPO_ROOT"))) Sys.getenv("REPO_ROOT") else
    if (length(f)) normalizePath(file.path(dirname(f[1]), ".."), winslash = "/") else getwd()
}
source(file.path(ROOT, "code", "functions.R"), encoding = "UTF-8")
dir.create(DIR_MAN, recursive = TRUE, showWarnings = FALSE)
rt <- function(n) rd_csv(file.path(DIR_TAB, paste0(n, ".csv")))
EM <- "\u2003"                                    # indentation for sub-rows
DASH <- "\u2014"
OUT <- list()
row_df <- function(rows, cols) { m <- do.call(rbind, rows); colnames(m) <- cols; as.data.frame(m, stringsAsFactors = FALSE) }

# ---------------------------------------------------------------- Table 1: sample characteristics
t1 <- rt("T1_samples")
key <- list(c("Study 1", "HV"), c("Study 1", "AV"), c("Study 2", "control"), c("Study 2", "ability"), c("Study 2", "rule_compliance"))
g <- lapply(key, function(k) t1[t1$study == k[1] & t1$group == k[2], ])
OUT$Table1 <- row_df(list(
  c("n", vapply(g, function(x) as.character(x$n), "")),
  c("Women, %", vapply(g, function(x) fmt(x$female_pct, 1), "")),
  c("Age, M (SD)", vapply(g, function(x) fmt_msd(x$age_M, x$age_SD), "")),
  c("Driving experience, years, M (SD)", vapply(g, function(x) fmt_msd(x$drive_yrs_M, x$drive_yrs_SD), ""))),
  c("", "Study 1: HV", "Study 1: AV", "Study 2: Control", "Study 2: Ability", "Study 2: Rule compliance"))

# ---------------------------------------------------------------- Table 2: Study 1 descriptives and AV - HV differences
te <- rt("S1_traits_expectations"); at <- rt("S1_attributions_overall"); ov <- rt("S1_overall_vehicle_effect")
P1 <- rd_csv(file.path(DIR_DAT, "study1_person.csv"))
r2 <- function(lab, x) c(lab, fmt_msd(x$M1, x$SD1), fmt_msd(x$M0, x$SD0), fmt_ci(x$diff, x$lo, x$hi), fmt(x$d), fmt_p(x$p))
rows <- list(c("Perceived characteristics", "", "", "", "", ""))
for (v in list(c("Overall safety, current (0-100)", "Overall safety, current (0\u2013100)"),
               c("Overall safety, expected (0-100)", "Overall safety, expected (0\u2013100)"),
               c("Safety gap (expected - current)", "Safety gap (expected \u2212 current)"),
               c("Driving ability, current (1-7)", "Driving ability, current (1\u20137)"),
               c("Driving ability, expected (1-7)", "Driving ability, expected (1\u20137)"),
               c("Ability gap (expected - current)", "Ability gap (expected \u2212 current)")))
  rows[[length(rows) + 1]] <- r2(v[2], te[te$variable == v[1], ])
rows[[length(rows) + 1]] <- c("Attributions (1\u201311)", "", "", "", "", "")
for (v in list(c("Trait (all internal-stable)", "Trait"), c("Competence-related trait", paste0(EM, "Competence-related")),
               c("Character-related trait", paste0(EM, "Character-related")),
               c("Character-related trait, identically worded items (4)", paste0(EM, "Character-related, identically worded items")),
               c("Momentary internal", "Momentary internal"), c("Situational", "Situational")))
  rows[[length(rows) + 1]] <- r2(v[2], at[at$attribution == v[1], ])
rows[[length(rows) + 1]] <- c("Reactions (1\u20137)", "", "", "", "", "")
for (v in list(c("anger", "Anger"), c("blame", "Blame"))) {
  y <- P1[[paste0("mean_", v[1])]]; o <- ov[ov$outcome == v[1], ]
  x <- list(M1 = mean(y[P1$AV == 1]), SD1 = sd(y[P1$AV == 1]), M0 = mean(y[P1$AV == 0]), SD0 = sd(y[P1$AV == 0]),
            diff = o$AV_minus_HV, lo = o$lo, hi = o$hi, d = o$d, p = o$p)
  rows[[length(rows) + 1]] <- r2(v[2], x)
}
OUT$Table2 <- row_df(rows, c("Variable", "AV M (SD)", "HV M (SD)", "AV \u2212 HV [95% CI]", "d", "p"))

# ---------------------------------------------------------------- Table 3: attribution-anger slopes
s1 <- rt("S1_multilevel_slopes"); s1 <- s1[s1$model == "A" & s1$outcome == "anger", ]
s2 <- rt("S2_multilevel_slopes"); s2 <- s2[s2$outcome == "anger", ]
rows <- list()
for (a in list(c("trait", "Trait"), c("momentary", "Momentary internal"), c("situational", "Situational")))
  for (lv in c("within", "between")) {
    x <- s1[s1$attribution == a[1] & s1$level == lv, ]; y <- s2[s2$attribution == a[1] & s2$level == lv, ]
    rows[[length(rows) + 1]] <- c(a[2], tools::toTitleCase(lv), fmt_ci(x$b_HV, x$b_HV_lo, x$b_HV_hi, 3),
                                  fmt_ci(x$b_AV, x$b_AV_lo, x$b_AV_hi, 3), fmt_ci(x$diff, x$diff_lo, x$diff_hi, 3),
                                  sprintf("%s (%s)", fmt_p(x$p_diff), fmt_p(x$p_diff_robust)), fmt_ci(y$b, y$lo, y$hi, 3))
  }
OUT$Table3 <- row_df(rows, c("Attribution", "Level", "Study 1 HV b [95% CI]", "Study 1 AV b [95% CI]", "AV \u2212 HV [95% CI]",
                             "p (robust p)", "Study 2 AV b [95% CI]"))

# ---------------------------------------------------------------- Table 4: decomposition
D <- rt("S1_decomposition_person_level"); red <- rt("S1_direct_effect_reduction")
specs <- c("attributions", "attributions + safety gap", "attributions + ability gap", "attributions + both gaps")
get <- function(s, t) D[D$spec == s & D$term == t, ]
rows <- list()
for (t in list(c("ind_mean_trait", "Indirect via trait"), c("ind_mean_momentary", "Indirect via momentary internal"),
               c("ind_mean_situational", "Indirect via situational"), c("ind_safety_gap", "Indirect via safety gap"),
               c("ind_ability_gap", "Indirect via ability gap"), c("ind_total", "Total indirect"), c("direct", "Direct effect (c\u2032)")))
  rows[[length(rows) + 1]] <- c(t[2], vapply(specs, function(s) { g <- get(s, t[1]); if (!nrow(g)) DASH else fmt_ci(g$est, g$lo, g$hi) }, ""))
rows[[length(rows) + 1]] <- c("Reduction in c\u2032 vs. attributions only", DASH, vapply(specs[-1], function(s) {
  x <- red[red$spec == s, ]; sprintf("%s (%s%%)", fmt_ci(x$reduction, x$lo, x$hi), fmt(100 * x$share, 0)) }, ""))
OUT$Table4 <- row_df(rows, c("Path", "Attributions only", "+ Safety gap", "+ Ability gap", "+ Both gaps"))
tot <- get("attributions", "total")
OUT$Table4_note <- data.frame(item = c("Total effect (AV \u2212 HV)", paste("a path,", c("mean_trait", "mean_momentary", "mean_situational", "safety_gap", "ability_gap"))),
                              value = c(fmt_ci(tot$est, tot$lo, tot$hi), vapply(c("a_mean_trait", "a_mean_momentary", "a_mean_situational", "a_safety_gap", "a_ability_gap"),
                                                                                function(t) fmt(get("attributions + both gaps", t)$est), "")),
                              stringsAsFactors = FALSE)
OUT$Table4_note <- rbind(OUT$Table4_note, data.frame(item = paste0("p (Holm), reduction with ", c("safety gap", "ability gap")),
  value = fmt_p(red$p_holm_gaps[match(c("attributions + safety gap", "attributions + ability gap"), red$spec)])))

# ---------------------------------------------------------------- Table 5: Study 2 information effects
g5 <- rt("S2_group_effects"); dec <- rt("S2_decomposition_person_level")
rows <- list()
for (o in list(c("trait", "Trait attribution"), c("trait_comp", paste0(EM, "Competence-related")),
               c("trait_char", paste0(EM, "Character-related (errors 4\u20135)")), c("momentary", "Momentary internal attribution"),
               c("situational", "Situational attribution"), c("anger", "Anger"), c("punishment", "Punishment"))) {
  rr <- o[2]
  for (cn in c("ability vs control", "rule compliance vs control")) {
    x <- g5[g5$outcome == o[1] & g5$contrast == cn, ]
    rr <- c(rr, fmt_ci(x$est, x$lo, x$hi), fmt(x$d), paste0(fmt_p(x$p), if (is.na(x$p_holm)) "" else sprintf(" (%s)", fmt_p(x$p_holm))))
  }
  x <- g5[g5$outcome == o[1] & g5$contrast == "rule compliance vs ability", ]
  rows[[length(rows) + 1]] <- c(rr, sprintf("%s, p = %s", fmt_ci(x$est, x$lo, x$hi), fmt_p(x$p)))
}
for (t in list(c("ind_mean_trait", "Indirect via trait attribution"), c("direct", "Direct effect"))) {
  rr <- t[2]
  for (cn in c("ability vs control", "rule compliance vs control")) {
    x <- dec[dec$contrast == cn & dec$term == t[1], ]; rr <- c(rr, fmt_ci(x$est, x$lo, x$hi), "", "")
  }
  rows[[length(rows) + 1]] <- c(rr, "")
}
OUT$Table5 <- row_df(rows, c("Outcome", "Ability vs. control: b [95% CI]", "d", "p (Holm)", "Rule compliance vs. control: b [95% CI]",
                             "d ", "p (Holm) ", "Rule compliance vs. ability: b [95% CI], p"))

# ================================================================ Supplementary tables
# S1: Study 1, AV - HV differences for each error
E <- rt("S1_by_event_attributions_anger_blame")
VAR <- list(c("trait", "Trait attribution"), c("trait_comp", paste0(EM, "Competence-related")), c("trait_char", paste0(EM, "Character-related")),
            c("momentary", "Momentary internal attribution"), c("situational", "Situational attribution"), c("anger", "Anger"), c("blame", "Blame"))
rows <- list()
for (no in unique(E$event_no)) {
  sub <- E[E$event_no == no, ]; both <- all(c("trait_comp", "trait_char") %in% sub$variable)
  rows[[length(rows) + 1]] <- c("group", sprintf("%s. %s", no, sub$event[1]), rep("", 6))
  for (v in VAR) {
    x <- sub[sub$variable == v[1], ]
    if (!nrow(x) || (v[1] %in% c("trait_comp", "trait_char") && !both)) next
    rows[[length(rows) + 1]] <- c("row", v[2], fmt_msd(x$M1, x$SD1), fmt_msd(x$M0, x$SD0), fmt_ci(x$diff, x$lo, x$hi),
                                  sprintf("%s (%d)", fmt(x$t), as.integer(x$df)), fmt_p(x$p), fmt(x$d))
  }
}
OUT$TableS1 <- row_df(rows, c("type", "Error / variable", "AV M (SD)", "HV M (SD)", "AV \u2212 HV [95% CI]", "t (df)", "p", "d"))

# S2: Study 1, decomposition for each error
BD <- rt("S1_decomposition_by_event")
rows <- lapply(unique(BD$event_no), function(no) {
  sub <- BD[BD$event_no == no, ]
  cell <- function(t) { x <- sub[sub$term == t, ]; if (!nrow(x) || is.na(x$est)) DASH else fmt_ci(x$est, x$lo, x$hi) }
  c(sprintf("%s. %s", no, sub$event[1]), cell("total"), cell("ind_trait"), cell("ind_momentary"), cell("ind_situational"), cell("direct"))
})
OUT$TableS2 <- row_df(rows, c("Error", "Total effect", "Indirect via trait", "Indirect via momentary internal", "Indirect via situational", "Direct effect"))

# S3: Study 2, adjusted information effects
A <- rt("S2_group_effects_adjusted")
rows <- lapply(list(c("trait", "Trait attribution"), c("situational", "Situational attribution"), c("anger", "Anger"), c("punishment", "Punishment")),
  function(o) {
    x1 <- A[A$outcome == o[1] & A$contrast == "ability vs control", ]; x2 <- A[A$outcome == o[1] & A$contrast == "rule compliance vs control", ]
    c(o[2], fmt_ci(x1$est, x1$lo, x1$hi), fmt_p(x1$p), fmt_ci(x2$est, x2$lo, x2$hi), fmt_p(x2$p))
  })
OUT$TableS3 <- row_df(rows, c("Outcome", "Ability vs. control: b [95% CI]", "p", "Rule compliance vs. control: b [95% CI]", "p "))

# S4: Study 2, effects for each error
SE <- rt("S2_by_event")
rows <- list()
for (no in unique(SE$event_no)) {
  sub <- SE[SE$event_no == no, ]
  rows[[length(rows) + 1]] <- c("group", sprintf("%s. %s", no, sub$event[1]), rep("", 9))
  for (o in list(c("trait", "Trait attribution"), c("anger", "Anger"), c("punishment", "Punishment"))) {
    x1 <- sub[sub$outcome == o[1] & sub$contrast == "ability vs control", ]; x2 <- sub[sub$outcome == o[1] & sub$contrast == "rule compliance vs control", ]
    rows[[length(rows) + 1]] <- c("row", paste0(EM, o[2]), fmt_msd(x1$M0, x1$SD0), fmt_msd(x1$M1, x1$SD1), fmt_msd(x2$M1, x2$SD1),
      fmt_ci(x1$diff, x1$lo, x1$hi), fmt(x1$d), sprintf("%s (%s)", fmt_p(x1$p), fmt_p(x1$p_fdr)),
      fmt_ci(x2$diff, x2$lo, x2$hi), fmt(x2$d), sprintf("%s (%s)", fmt_p(x2$p), fmt_p(x2$p_fdr)))
  }
}
OUT$TableS4 <- row_df(rows, c("type", "Error / outcome", "Control M (SD)", "Ability M (SD)", "Rule compliance M (SD)",
                              "Ability \u2212 control [95% CI]", "d", "p (FDR)", "Rule compliance \u2212 control [95% CI]", "d ", "p (FDR) "))

# S5: Study 1, slopes for blame and with the trait split
M <- rt("S1_multilevel_slopes")
LAB <- c(trait = "Trait", trait_comp = "Competence-related trait", trait_char = "Character-related trait",
         momentary = "Momentary internal", situational = "Situational")
rows <- list()
for (pn in list(c("A", "blame", "A. Blame, three attributions"),
                c("B", "anger", "B. Anger, competence- and character-related trait attributions entered separately"),
                c("B", "blame", "C. Blame, competence- and character-related trait attributions entered separately"))) {
  rows[[length(rows) + 1]] <- c("panel", pn[3], rep("", 5))
  sub <- M[M$model == pn[1] & M$outcome == pn[2], ]
  for (i in seq_len(nrow(sub))) {
    x <- sub[i, ]
    rows[[length(rows) + 1]] <- c("row", LAB[[x$attribution]], tools::toTitleCase(x$level), fmt_ci(x$b_HV, x$b_HV_lo, x$b_HV_hi, 3),
      fmt_ci(x$b_AV, x$b_AV_lo, x$b_AV_hi, 3), fmt_ci(x$diff, x$diff_lo, x$diff_hi, 3),
      sprintf("%s (%s)", fmt_p(x$p_diff), fmt_p(x$p_diff_robust)))
  }
}
OUT$TableS5 <- row_df(rows, c("type", "Attribution", "Level", "HV b [95% CI]", "AV b [95% CI]", "AV \u2212 HV [95% CI]", "p (robust p)"))

# S6: Study 2, manipulation check
C6 <- rt("S2_manipulation_check")
LABC <- c("ability pre-test, 3 groups" = "Perceived driving ability before the information, three conditions",
          "ability after message (control: pre-test)" = "Perceived driving ability after the information (control: pre-test rating)",
          "ability change within ability message" = "Change in perceived driving ability, ability information",
          "ability change within rule-compliance message" = "Change in perceived driving ability, rule-compliance information",
          "normativity pre-test, 3 groups" = "Perceived rule compliance before the information, three conditions",
          "normativity after message (control: pre-test)" = "Perceived rule compliance after the information (control: pre-test rating)",
          "normativity change within ability message" = "Change in perceived rule compliance, ability information",
          "normativity change within rule-compliance message" = "Change in perceived rule compliance, rule-compliance information")
detail <- function(s) {
  s <- sub("rule:", "rule compliance:", s, fixed = TRUE); s <- sub("change ", "change = ", s, fixed = TRUE)
  s <- sub("pre ", "pre = ", s, fixed = TRUE); s <- sub("post ", "post = ", s, fixed = TRUE)
  gsub("-", MINUS, s, fixed = TRUE)
}
OUT$TableS6 <- row_df(lapply(seq_len(nrow(C6)), function(i) c(LABC[[C6$check[i]]], C6$stat[i], fmt_p(C6$p[i]), detail(C6$detail[i]))),
                      c("Comparison", "Test", "p", "Means"))

# ---------------------------------------------------------------- write
md <- character(0)
for (nm in names(OUT)) {
  d <- OUT[[nm]]
  wr_csv(d, file.path(DIR_MAN, paste0(nm, ".csv")))
  md <- c(md, paste("##", nm), "", paste("|", paste(names(d), collapse = " | "), "|"),
          paste0(paste(rep("|---", ncol(d)), collapse = ""), "|"),
          apply(d, 1, function(r) paste("|", paste(r, collapse = " | "), "|")), "")
}
writeLines(enc2utf8(md), file.path(DIR_MAN, "manuscript_tables.md"), useBytes = TRUE)
cat("written:", paste(names(OUT), collapse = ", "), "\n")
