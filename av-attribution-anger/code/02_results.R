# =============================================================================
# Step 2: every number in the Results section, Tables 1-5 and Supplementary Tables S1-S6.
#
# Input : data/processed/*.csv (from 01_prepare_data.R)
# Output: output/tables/*.csv (one file per result table; results_tables.xlsx if openxlsx is installed)
# Mixed models: lme4 (random intercept for participants, fixed effects for errors, REML), t tests with
#   Satterthwaite degrees of freedom (lmerTest), CR2 cluster-robust SEs clustered by participant (clubSandwich).
# Bootstrap: 2,000 resamples (1,000 per error for the error-specific decompositions), stratified by
#   condition; the seed 20261118 is set before each bootstrap analysis, so all results are reproducible.
# Run   : Rscript code/02_results.R   (several minutes, mostly for the CR2 standard errors)
# =============================================================================
if (!exists("ROOT")) {
  a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  ROOT <- if (nzchar(Sys.getenv("REPO_ROOT"))) Sys.getenv("REPO_ROOT") else
    if (length(f)) normalizePath(file.path(dirname(f[1]), ".."), winslash = "/") else getwd()
}
source(file.path(ROOT, "code", "functions.R"), encoding = "UTF-8")
dir.create(DIR_TAB, recursive = TRUE, showWarnings = FALSE)
TABLES <- list()
save_tab <- function(d, name) { wr_csv(d, file.path(DIR_TAB, paste0(name, ".csv"))); TABLES[[name]] <<- d; invisible(d) }
say <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

S1L <- rd_csv(file.path(DIR_DAT, "study1_long.csv")); P1 <- rd_csv(file.path(DIR_DAT, "study1_person.csv"))
S2L <- rd_csv(file.path(DIR_DAT, "study2_long.csv")); P2 <- rd_csv(file.path(DIR_DAT, "study2_person.csv"))
S1L$event_no <- as.character(S1L$event_no); S2L$event_no <- as.character(S2L$event_no)
EV1 <- S1L[!duplicated(S1L$event_code), c("event_code", "event_no", "event")]
EV2 <- S2L[!duplicated(S2L$event_code), c("event_code", "event_no", "event")]
av <- P1$AV == 1; hv <- P1$AV == 0
ATTR <- c("trait", "momentary", "situational")

# ============================================================ Table 1: samples
say("Table 1")
t1 <- do.call(rbind, lapply(list(list("Study 1", P1, "vehicle", c("HV", "AV")),
                                 list("Study 2", P2, "group", c("control", "ability", "rule_compliance"))), function(s) {
  do.call(rbind, lapply(c(s[[4]], "all"), function(lv) {
    q <- if (lv == "all") s[[2]] else s[[2]][s[[2]][[s[[3]]]] == lv, ]
    data.frame(study = s[[1]], group = lv, n = nrow(q), female_n = sum(q$female), female_pct = 100 * mean(q$female),
               age_M = mean(q$age), age_SD = sd(q$age), age_min = min(q$age), age_max = max(q$age),
               drive_yrs_M = mean(q$driving_years), drive_yrs_SD = sd(q$driving_years))
  }))
}))
bal <- rbind(
  do.call(rbind, lapply(c("female", "age", "driving_years", "mileage_10k_km"), function(v) {
    r <- tt(P1[av, v], P1[hv, v]); data.frame(study = "Study 1", variable = v, test = sprintf("t(%d)", r$df), stat = r$t, p = r$p)
  })),
  do.call(rbind, lapply(c("female", "age", "driving_years"), function(v) {
    a <- anova(lm(P2[[v]] ~ factor(P2$group_code)))
    data.frame(study = "Study 2", variable = v, test = sprintf("F(%d,%d)", a$Df[1], a$Df[2]), stat = a[["F value"]][1], p = a[["Pr(>F)"]][1])
  })))
save_tab(t1, "T1_samples"); save_tab(bal, "T1b_balance")

# ============================================================ Study 1: perceived characteristics and expectations
say("Study 1 descriptives")
vars <- c(safety_current = "Overall safety, current (0-100)", safety_expected = "Overall safety, expected (0-100)",
          safety_gap = "Safety gap (expected - current)", ability_current = "Driving ability, current (1-7)",
          ability_expected = "Driving ability, expected (1-7)", ability_gap = "Ability gap (expected - current)")
save_tab(do.call(rbind, lapply(names(vars), function(v) cbind(variable = vars[[v]], tt(P1[av, v], P1[hv, v])))),
         "S1_traits_expectations")

# ============================================================ Study 1: attributions (RQ1)
vars <- c(mean_trait_char = "Character-related trait", mean_trait_comp = "Competence-related trait",
          mean_trait = "Trait (all internal-stable)", mean_momentary = "Momentary internal", mean_situational = "Situational")
att <- do.call(rbind, lapply(names(vars), function(v) cbind(attribution = vars[[v]], tt(P1[av, v], P1[hv, v]))))
# identically worded character items in both versions: CA02 cause 2, CS02 cause 1, WA01 causes 1 and 2
# (WA01 trait_char is the mean of its two causes, so it receives weight 2)
w <- c(CA02 = 1, CS02 = 1, WA01 = 2)
q <- S1L[S1L$event_code %in% names(w), ]; q$w <- w[q$event_code]
idm <- sapply(split(q, q$pid), function(x) weighted.mean(x$trait_char, x$w))[as.character(P1$pid)]
att <- rbind(att, cbind(attribution = "Character-related trait, identically worded items (4)", tt(idm[av], idm[hv])))
save_tab(att, "S1_attributions_overall")

S1E <- do.call(rbind, lapply(seq_len(nrow(EV1)), function(i) {
  q <- S1L[S1L$event_code == EV1$event_code[i], ]
  do.call(rbind, lapply(c("trait", "trait_char", "trait_comp", "momentary", "situational", "anger", "blame"), function(v) {
    if (all(is.na(q[[v]]))) return(NULL)
    cbind(event_no = EV1$event_no[i], event = EV1$event[i], variable = v, tt(q[q$AV == 1, v], q[q$AV == 0, v]))
  }))
}))
save_tab(S1E, "S1_by_event_attributions_anger_blame")
# across the ten conditions: is the AV-HV difference in trait attribution related to the difference in anger?
dtr <- S1E$d[S1E$variable == "trait"]; dan <- S1E$d[S1E$variable == "anger"]
ct <- cor.test(dtr, dan)
save_tab(data.frame(x = "AV-HV d, trait attribution", y = "AV-HV d, anger", k = length(dtr), r = unname(ct$estimate), p = ct$p.value),
         "S1_trait_anger_difference_correlation")

# ============================================================ Study 1: anger and blame, overall (mixed model)
say("Study 1 overall vehicle effect")
L <- S1L; L$event <- factor(L$event_code)
save_tab(do.call(rbind, lapply(c("anger", "blame"), function(out) {
  dd <- L[!is.na(L[[out]]), ]
  f <- fit_lmm(as.formula(paste(out, "~ AV + event + (1 | pid)")), dd)
  e <- lin(f, c(AV = 1), vcov_cr2(f, dd$pid))
  data.frame(outcome = out, AV_minus_HV = e$est, lo = e$lo, hi = e$hi, df = e$df, p = e$p, p_robust = e$p_robust,
             d = e$est / sd(P1[[paste0("mean_", out)]]))
})), "S1_overall_vehicle_effect")

# ============================================================ Study 1: heterogeneity across errors (anger, d scale)
say("Study 1 heterogeneity")
piv <- tapply(S1L$anger, list(S1L$pid, S1L$event_code), mean)[as.character(P1$pid), EV1$event_code]
d_events <- function(idx) {
  M <- piv[idx, , drop = FALSE]; a <- P1$AV[idx] == 1
  m1 <- colMeans(M[a, , drop = FALSE]); m0 <- colMeans(M[!a, , drop = FALSE])
  v1 <- apply(M[a, , drop = FALSE], 2, var); v0 <- apply(M[!a, , drop = FALSE], 2, var)
  n1 <- sum(a); n0 <- sum(!a)
  (m1 - m0) / sqrt(((n1 - 1) * v1 + (n0 - 1) * v0) / (n1 + n0 - 2))
}
set.seed(SEED)
d_obs <- d_events(seq_len(nrow(P1)))
B <- t(vapply(seq_len(NBOOT), function(i) d_events(strat_idx(P1$AV)), numeric(length(d_obs))))
k <- length(d_obs); se2 <- apply(B, 2, var); mu <- mean(d_obs); se_mu <- sd(rowMeans(B))
tau2 <- max(0, var(d_obs) - mean(se2))
taus <- sqrt(pmax(0, apply(B, 1, var) - mean(se2)))
qk <- qt(.975, k - 2)
save_tab(data.frame(k = k, mean_d = mu, mean_d_lo = quantile(rowMeans(B), .025, names = FALSE),
                    mean_d_hi = quantile(rowMeans(B), .975, names = FALSE), tau_d = sqrt(tau2),
                    tau_lo = quantile(taus, .025, names = FALSE), tau_hi = quantile(taus, .975, names = FALSE),
                    PI_lo = mu - qk * sqrt(tau2 + se_mu^2), PI_hi = mu + qk * sqrt(tau2 + se_mu^2),
                    true_variance_share = tau2 / var(d_obs)), "S1_anger_heterogeneity")

# ============================================================ Study 1: attribution-anger slopes (RQ2)
# outcome ~ AV + error + sum over attributions of [W + B + AV x W + AV x B] + (1 | pid)
# coefficient of x = slope for HV; x + AVx = slope for AV; AVx = AV - HV difference
add_terms <- function(d, mv, interact = TRUE) {
  rhs <- character(0)
  for (m in mv) for (lv in c("W", "B")) {
    x <- paste0(m, "_", lv); rhs <- c(rhs, x)
    if (interact) { d[[paste0("AVx", x)]] <- d$AV * d[[x]]; rhs <- c(rhs, paste0("AVx", x)) }
  }
  list(d = d, rhs = rhs)
}
s1_model <- function(L, mv, out, extra = NULL) {
  dd <- decompose(L, mv)
  if (!is.null(extra)) dd <- extra(dd)
  at <- add_terms(dd, mv); dd <- at$d
  dd <- dd[!is.na(dd[[out]]), ]; dd$event <- factor(dd$event_code)
  rhs <- c("AV", "event", at$rhs, attr(extra, "terms"))
  f <- fit_lmm(as.formula(paste(out, "~", paste(rhs, collapse = " + "), "+ (1 | pid)")), dd)
  list(fit = f, data = dd)
}
say("Study 1 slopes (4 models)")
slopes <- list(); iccs <- list()
for (model in c("A", "B")) {
  mv <- if (model == "A") ATTR else c("trait_comp", "trait_char", "momentary", "situational")
  for (out in c("anger", "blame")) {
    s <- s1_model(S1L, mv, out); V <- vcov_cr2(s$fit, s$data$pid)
    for (m in mv) for (lv in c("W", "B")) {
      x <- paste0(m, "_", lv); ax <- paste0("AVx", x)
      h <- lin(s$fit, setNames(1, x)); a <- lin(s$fit, setNames(c(1, 1), c(x, ax))); dd <- lin(s$fit, setNames(1, ax), V)
      slopes[[length(slopes) + 1]] <- data.frame(
        model = model, outcome = out, attribution = m, level = ifelse(lv == "W", "within", "between"),
        b_HV = h$est, b_HV_lo = h$lo, b_HV_hi = h$hi, p_HV = h$p, b_AV = a$est, b_AV_lo = a$lo, b_AV_hi = a$hi, p_AV = a$p,
        diff = dd$est, diff_lo = dd$lo, diff_hi = dd$hi, p_diff = dd$p, p_diff_robust = dd$p_robust)
    }
    if (out == "anger") iccs[[length(iccs) + 1]] <- data.frame(model = model, ICC = icc(s$fit), N_obs = nrow(s$data),
                                                               N_persons = length(unique(s$data$pid)))
  }
}
save_tab(do.call(rbind, slopes), "S1_multilevel_slopes")
save_tab(do.call(rbind, iccs), "S1_multilevel_icc")

# ============================================================ Study 1: decomposition (RQ3)
say("Study 1 decomposition (bootstrap)")
P1c <- P1
for (m in c("mean_trait", "mean_momentary", "mean_situational", "mean_trait_comp", "mean_trait_char", "safety_gap", "ability_gap"))
  P1c[[m]] <- P1c[[m]] - mean(P1c[[m]])
P1c$safety_gap <- P1c$safety_gap / 10                       # per 10 points
base <- c("mean_trait", "mean_momentary", "mean_situational")
specs <- list("attributions" = base, "attributions + safety gap" = c(base, "safety_gap"),
              "attributions + ability gap" = c(base, "ability_gap"), "attributions + both gaps" = c(base, "safety_gap", "ability_gap"),
              "attributions (trait split)" = c("mean_trait_comp", "mean_trait_char", "mean_momentary", "mean_situational"))
D <- do.call(rbind, lapply(names(specs), function(s) {
  set.seed(SEED); cbind(spec = s, boot_med(P1c, "AV", specs[[s]], "mean_anger"))
}))
save_tab(D, "S1_decomposition_person_level")

# reduction of the direct effect relative to the attributions-only model, with shared resamples
direct <- function(Q, mv) { r <- med(Q, "AV", mv, "mean_anger"); r[length(r) - 1] }
others <- setdiff(names(specs), "attributions")
set.seed(SEED)
R <- t(vapply(seq_len(NBOOT), function(i) {
  Q <- P1c[strat_idx(P1c$AV), ]; d0 <- direct(Q, base)
  vapply(others, function(s) d0 - direct(Q, specs[[s]]), numeric(1))
}, numeric(length(others))))
colnames(R) <- others
d0 <- direct(P1c, base)
red <- do.call(rbind, lapply(others, function(s) {
  d1 <- direct(P1c, specs[[s]]); r <- R[, s]
  data.frame(spec = s, direct_base = d0, direct = d1, reduction = d0 - d1,
             lo = quantile(r, .025, names = FALSE), hi = quantile(r, .975, names = FALSE),
             p_boot = 2 * min(mean(r <= 0), mean(r >= 0)), share = (d0 - d1) / d0)
}))
gi <- red$spec %in% c("attributions + safety gap", "attributions + ability gap")
red$p_holm_gaps <- NA_real_; red$p_holm_gaps[gi] <- p.adjust(red$p_boot[gi], "holm")
save_tab(red, "S1_direct_effect_reduction")

# expectation gap x vehicle type in the mixed model (anger ~ model A + gap + AV x gap)
say("Study 1 gap moderation")
gm <- do.call(rbind, lapply(c("safety_gap", "ability_gap"), function(g) {
  extra <- function(dd) { dd$GAP <- P1c[[g]][match(dd$pid, P1c$pid)]; dd$AVxGAP <- dd$AV * dd$GAP; dd }
  attr(extra, "terms") <- c("GAP", "AVxGAP")
  s <- s1_model(S1L, ATTR, "anger", extra); V <- vcov_cr2(s$fit, s$data$pid)
  do.call(rbind, lapply(list(list("gap slope, HV", c(GAP = 1)), list("gap slope, AV", c(GAP = 1, AVxGAP = 1)),
                             list("AV x gap", c(AVxGAP = 1))), function(t) {
    e <- lin(s$fit, t[[2]], V)
    data.frame(gap = g, term = t[[1]], est = e$est, lo = e$lo, hi = e$hi, p = e$p, p_robust = e$p_robust)
  }))
}))
save_tab(gm, "S1_gap_moderation")

# error-specific decomposition (1,000 resamples per error)
say("Study 1 decomposition by error (bootstrap)")
save_tab(do.call(rbind, lapply(seq_len(nrow(EV1)), function(i) {
  q <- S1L[S1L$event_code == EV1$event_code[i], ]
  mv <- ATTR[vapply(ATTR, function(m) any(!is.na(q[[m]])), logical(1))]
  for (m in mv) q[[m]] <- q[[m]] - mean(q[[m]])
  set.seed(SEED)
  cbind(event_no = EV1$event_no[i], event = EV1$event[i], boot_med(q, "AV", mv, "anger", nboot = 1000))
})), "S1_decomposition_by_event")

# ============================================================ Study 2
say("Study 2 manipulation check")
P2$grp <- P2$group_code
gl <- c("control", "ability", "rule")
mc <- list()
for (v in c("ability", "normativity")) {
  pre <- P2[[paste0(v, "_pre")]]; postv <- P2[[paste0(v, "_post")]]
  a <- anova(lm(pre ~ factor(P2$grp)))
  mc[[length(mc) + 1]] <- data.frame(check = sprintf("%s pre-test, 3 groups", v),
    stat = sprintf("F(%d,%d) = %.2f", a$Df[1], a$Df[2], a[["F value"]][1]), p = a[["Pr(>F)"]][1],
    detail = paste(sprintf("%s: %.2f", gl, tapply(pre, P2$grp, mean)), collapse = "; "))
  post <- ifelse(P2$grp == 0, pre, postv)
  a <- anova(lm(post ~ factor(P2$grp)))
  mc[[length(mc) + 1]] <- data.frame(check = sprintf("%s after message (control: pre-test)", v),
    stat = sprintf("F(%d,%d) = %.2f", a$Df[1], a$Df[2], a[["F value"]][1]), p = a[["Pr(>F)"]][1],
    detail = paste(sprintf("%s: %.2f", gl, tapply(post, P2$grp, mean)), collapse = "; "))
  for (kk in 1:2) {
    i <- P2$grp == kk; dl <- postv[i] - pre[i]; tp <- t.test(postv[i], pre[i], paired = TRUE)
    mc[[length(mc) + 1]] <- data.frame(check = sprintf("%s change within %s", v, c("ability message", "rule-compliance message")[kk]),
      stat = sprintf("t(%d) = %.2f", sum(i) - 1, tp$statistic), p = tp$p.value,
      detail = sprintf("pre %.2f, post %.2f, change %.2f, dz = %.2f", mean(pre[i]), mean(postv[i]), mean(dl), mean(dl) / sd(dl)))
  }
}
save_tab(do.call(rbind, mc), "S2_manipulation_check")

# information effects: outcome ~ G1 + G2 + error + (1 | pid); control as reference
say("Study 2 information effects")
sd_pool <- function(out) sqrt(mean(tapply(P2[[paste0("mean_", out)]], P2$grp, var, na.rm = TRUE)))
s2_data <- function(out, covars = NULL) {
  dd <- S2L[!is.na(S2L[[out]]), ]
  dd$event <- factor(dd$event_code)
  dd$G1 <- as.numeric(dd$group_code == 1); dd$G2 <- as.numeric(dd$group_code == 2)
  for (v in covars) dd[[v]] <- P2[[v]][match(dd$pid, P2$pid)] - mean(P2[[v]])
  dd
}
CONTR <- list("ability vs control" = c(G1 = 1), "rule compliance vs control" = c(G2 = 1),
              "rule compliance vs ability" = c(G2 = 1, G1 = -1))
S2G <- do.call(rbind, lapply(c("trait", "trait_comp", "trait_char", "momentary", "situational", "anger", "punishment"), function(out) {
  dd <- s2_data(out); f <- fit_lmm(as.formula(paste(out, "~ G1 + G2 + event + (1 | pid)")), dd)
  V <- vcov_cr2(f, dd$pid); s <- sd_pool(out)
  do.call(rbind, lapply(names(CONTR), function(cn) {
    e <- lin(f, CONTR[[cn]], V)
    data.frame(outcome = out, contrast = cn, est = e$est, lo = e$lo, hi = e$hi, df = e$df, p = e$p, p_robust = e$p_robust,
               d = e$est / s, d_lo = e$lo / s, d_hi = e$hi / s)
  }))
}))
fam <- S2G$outcome %in% c("trait", "situational", "anger", "punishment") & S2G$contrast != "rule compliance vs ability"
S2G$p_holm <- NA_real_; S2G$p_holm[fam] <- p.adjust(S2G$p[fam], "holm")
save_tab(S2G, "S2_group_effects")

# sensitivity: adjusted for age, driving experience and gender (grand-mean centred)
save_tab(do.call(rbind, lapply(c("trait", "situational", "anger", "punishment"), function(out) {
  dd <- s2_data(out, c("age", "driving_years", "female"))
  f <- fit_lmm(as.formula(paste(out, "~ G1 + G2 + age + driving_years + female + event + (1 | pid)")), dd)
  do.call(rbind, lapply(names(CONTR)[1:2], function(cn) {
    e <- lin(f, CONTR[[cn]])
    data.frame(outcome = out, contrast = cn, est = e$est, lo = e$lo, hi = e$hi, df = e$df, p = e$p)
  }))
})), "S2_group_effects_adjusted")

# error-specific comparisons (anger, punishment, trait attribution); BH-FDR within each outcome
S2E <- do.call(rbind, lapply(seq_len(nrow(EV2)), function(i) {
  q <- S2L[S2L$event_code == EV2$event_code[i], ]
  do.call(rbind, lapply(c("anger", "punishment", "trait"), function(out) do.call(rbind, lapply(1:2, function(kk) {
    cbind(event_no = EV2$event_no[i], event = EV2$event[i], outcome = out,
          contrast = c("ability vs control", "rule compliance vs control")[kk],
          tt(q[q$group_code == kk, out], q[q$group_code == 0, out]))
  }))))
}))
S2E$p_fdr <- NA_real_
for (out in c("anger", "punishment", "trait")) { i <- S2E$outcome == out; S2E$p_fdr[i] <- p.adjust(S2E$p[i], "BH") }
save_tab(S2E, "S2_by_event")

# person-level decomposition of each information effect (no X x M interaction)
say("Study 2 decomposition (bootstrap)")
P2c <- P2
for (m in c("mean_trait", "mean_momentary", "mean_situational")) P2c[[m]] <- P2c[[m]] - mean(P2c[[m]])
save_tab(do.call(rbind, lapply(1:2, function(kk) {
  Q <- P2c[P2c$grp %in% c(0, kk), ]; Q$X <- as.numeric(Q$grp == kk)
  set.seed(SEED)
  cbind(contrast = c("ability vs control", "rule compliance vs control")[kk],
        boot_med(Q, "X", c("mean_trait", "mean_momentary", "mean_situational"), "mean_anger", moderated = FALSE))
})), "S2_decomposition_person_level")

# within- and between-person slopes in Study 2
say("Study 2 slopes")
d2 <- decompose(S2L, ATTR); d2$event <- factor(d2$event_code)
d2$G1 <- as.numeric(d2$group_code == 1); d2$G2 <- as.numeric(d2$group_code == 2)
preds <- paste0(rep(ATTR, each = 2), c("_W", "_B"))
sl2 <- list()
for (out in c("anger", "punishment")) {
  f <- fit_lmm(as.formula(paste(out, "~ G1 + G2 + event +", paste(preds, collapse = " + "), "+ (1 | pid)")), d2)
  for (x in preds) {
    e <- lin(f, setNames(1, x))
    sl2[[length(sl2) + 1]] <- data.frame(outcome = out, attribution = sub("_[WB]$", "", x),
                                         level = ifelse(grepl("_W$", x), "within", "between"), b = e$est, lo = e$lo, hi = e$hi, p = e$p)
  }
  if (out == "anger") icc2 <- icc(f)
}
sl2[[length(sl2) + 1]] <- data.frame(outcome = "anger", attribution = "ICC", level = "", b = icc2, lo = NA, hi = NA, p = NA)
save_tab(do.call(rbind, sl2), "S2_multilevel_slopes")

# ============================================================ workbook and session information
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(TABLES)) { sh <- substr(nm, 1, 31); openxlsx::addWorksheet(wb, sh); openxlsx::writeData(wb, sh, TABLES[[nm]]) }
  openxlsx::saveWorkbook(wb, file.path(DIR_TAB, "results_tables.xlsx"), overwrite = TRUE)
} else message("openxlsx not installed: results_tables.xlsx not written (the CSV files contain all results).")
si <- c(R.version.string, vapply(c(PKGS, "Matrix"), function(p) paste(p, as.character(packageVersion(p))), character(1)))
writeLines(si, file.path(DIR_TAB, "session_info.txt"))
say("saved", length(TABLES), "tables;", paste(si, collapse = "; "))
