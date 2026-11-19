# =============================================================================
# Step 4 (robustness check, not reported in the current manuscript draft): wording of the Study 1 blame item.
#
# In the AV version of the Study 1 questionnaire, the blame item for three conditions (CA01 slow vehicle
# ahead, CA02 sudden braking ahead, CA03 stopping behind an obstacle) still read "how would you blame the
# driver of the vehicle ahead", so the target of blame differed between versions; the other seven conditions
# used neutral wording ("the vehicle ...") or named the AV. This script re-estimates the overall AV-HV blame
# difference (Section 3.4) and the blame slopes (Section 3.5, model A) without these three conditions, and
# checks whether anger and blame were less closely related in the AV group in the affected conditions. Anger
# is re-estimated on the same seven conditions for reference only (the anger item had no wording problem).
#
# Output: output/tables/S1_blame_wording_robustness_overall.csv, _slopes.csv, _anger_blame_corr.csv
# Run   : Rscript code/04_robustness_blame_wording.R
# =============================================================================
if (!exists("ROOT")) {
  a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  ROOT <- if (nzchar(Sys.getenv("REPO_ROOT"))) Sys.getenv("REPO_ROOT") else
    if (length(f)) normalizePath(file.path(dirname(f[1]), ".."), winslash = "/") else getwd()
}
source(file.path(ROOT, "code", "functions.R"), encoding = "UTF-8")
L_ALL <- rd_csv(file.path(DIR_DAT, "study1_long.csv"))
AFFECTED <- c("CA01", "CA02", "CA03")
SETS <- list("all 10 conditions" = L_ALL,
             "7 conditions (CA01-CA03 excluded)" = L_ALL[!L_ALL$event_code %in% AFFECTED, ],
             "CA01-CA03 only" = L_ALL[L_ALL$event_code %in% AFFECTED, ])
ATTR <- c("trait", "momentary", "situational")

overall <- do.call(rbind, lapply(names(SETS), function(lab) {
  L <- SETS[[lab]]; L$event <- factor(L$event_code)
  do.call(rbind, lapply(c("anger", "blame"), function(out) {
    f <- fit_lmm(as.formula(paste(out, "~ AV + event + (1 | pid)")), L)
    e <- lin(f, c(AV = 1), vcov_cr2(f, L$pid))
    pm <- gmean(L[[out]], L$pid)
    data.frame(set = lab, k_conditions = length(unique(L$event_code)), outcome = out, AV_minus_HV = e$est,
               lo = e$lo, hi = e$hi, p = e$p, p_robust = e$p_robust, d = e$est / sd(pm))
  }))
}))

slopes <- do.call(rbind, lapply(names(SETS)[1:2], function(lab) do.call(rbind, lapply(c("blame", "anger"), function(out) {
  dd <- decompose(SETS[[lab]], ATTR); rhs <- character(0)
  for (m in ATTR) for (lv in c("W", "B")) {
    x <- paste0(m, "_", lv); dd[[paste0("AVx", x)]] <- dd$AV * dd[[x]]; rhs <- c(rhs, x, paste0("AVx", x))
  }
  dd <- dd[!is.na(dd[[out]]), ]; dd$event <- factor(dd$event_code)
  f <- fit_lmm(as.formula(paste(out, "~ AV + event +", paste(rhs, collapse = " + "), "+ (1 | pid)")), dd)
  V <- vcov_cr2(f, dd$pid)
  do.call(rbind, lapply(ATTR, function(m) do.call(rbind, lapply(c("W", "B"), function(lv) {
    x <- paste0(m, "_", lv); ax <- paste0("AVx", x)
    h <- lin(f, setNames(1, x)); a <- lin(f, setNames(c(1, 1), c(x, ax))); d <- lin(f, setNames(1, ax), V)
    data.frame(set = lab, outcome = out, attribution = m, level = ifelse(lv == "W", "within", "between"),
               b_HV = h$est, p_HV = h$p, b_AV = a$est, p_AV = a$p, diff = d$est, diff_lo = d$lo, diff_hi = d$hi,
               p_diff = d$p, p_diff_robust = d$p_robust)
  }))))
}))))

# diagnostic: within-condition anger-blame correlation by vehicle type
corr <- do.call(rbind, lapply(unique(L_ALL$event_code), function(ev) {
  q <- L_ALL[L_ALL$event_code == ev, ]
  r <- sapply(0:1, function(v) cor(q$anger[q$AV == v], q$blame[q$AV == v]))
  n <- sapply(0:1, function(v) sum(q$AV == v))
  z <- (atanh(r[2]) - atanh(r[1])) / sqrt(1 / (n[2] - 3) + 1 / (n[1] - 3))
  data.frame(event_code = ev, event = q$event[1], affected_wording = ev %in% AFFECTED, r_HV = r[1], r_AV = r[2],
             diff_r = r[2] - r[1], z = z, p = 2 * pnorm(-abs(z)))
}))

wr_csv(overall, file.path(DIR_TAB, "S1_blame_wording_robustness_overall.csv"))
wr_csv(slopes, file.path(DIR_TAB, "S1_blame_wording_robustness_slopes.csv"))
wr_csv(corr, file.path(DIR_TAB, "S1_blame_wording_robustness_anger_blame_corr.csv"))
print(overall, digits = 3, row.names = FALSE); cat("\n")
print(subset(slopes, attribution == "situational"), digits = 3, row.names = FALSE); cat("\n")
print(corr, digits = 3, row.names = FALSE)
