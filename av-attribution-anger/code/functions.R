# =============================================================================
# Shared helpers for all analysis scripts (sourced by 00-05).
# =============================================================================

# ---- repository root ---------------------------------------------------------
# Order: an existing ROOT object, the REPO_ROOT environment variable, the folder of the
# script given to Rscript, otherwise the working directory (or its parent if it is code/).
find_root <- function() {
  r <- Sys.getenv("REPO_ROOT", unset = "")
  if (nzchar(r)) return(normalizePath(r, winslash = "/"))
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) return(normalizePath(file.path(dirname(f[1]), ".."), winslash = "/"))
  wd <- getwd()
  if (basename(wd) == "code") dirname(wd) else wd
}
if (!exists("ROOT", inherits = TRUE)) ROOT <- find_root()
if (!dir.exists(file.path(ROOT, "code"))) {
  stop("Repository root not found. Run the scripts from the repository root, e.g. ",
       "Rscript code/run_all.R, or set the environment variable REPO_ROOT.")
}

DIR_SRC <- file.path(ROOT, "data", "source")
DIR_DAT <- file.path(ROOT, "data", "processed")
DIR_TAB <- file.path(ROOT, "output", "tables")
DIR_FIG <- file.path(ROOT, "output", "figures")
DIR_MAN <- file.path(ROOT, "output", "manuscript_tables")
SEED  <- 20261118
NBOOT <- 2000

# ---- input / output ------------------------------------------------------------
rd_csv <- function(path) {
  read.csv(path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE,
           na.strings = c("", " ", "NA"))
}
wr_csv <- function(d, path) {
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  for (j in seq_along(d)) if (is.numeric(d[[j]])) d[[j]][is.nan(d[[j]])] <- NA
  write.csv(d, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(d)
}
rmean <- function(df, cols) {             # row means over the given columns, missing values skipped
  if (!length(cols)) return(rep(NA_real_, nrow(df)))
  m <- rowMeans(as.matrix(df[, cols, drop = FALSE]), na.rm = TRUE)
  m[is.nan(m)] <- NA
  m
}
gmean <- function(x, g) {                 # mean of x within each level of g (missing skipped), by unique g
  m <- tapply(x, g, mean, na.rm = TRUE)
  m[is.nan(m)] <- NA
  m
}

# ---- two-group comparison ------------------------------------------------------------
# Student t test of x - y (equal variances), Cohen's d with the pooled SD, 95% CI of the difference.
tt <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  nx <- length(x); ny <- length(y)
  tst <- t.test(x, y, var.equal = TRUE)
  sp <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  diff <- mean(x) - mean(y)
  data.frame(M1 = mean(x), SD1 = sd(x), M0 = mean(y), SD0 = sd(y), diff = diff,
             lo = tst$conf.int[1], hi = tst$conf.int[2], t = unname(tst$statistic),
             df = nx + ny - 2, p = tst$p.value, d = diff / sp)
}

# ---- within/between decomposition of error-level attributions ----------------------------
# c = x - error mean; B = participant mean of c, centred on the mean across participants;
# W = c - participant mean of c (set to 0 where the class was not measured for an error).
decompose <- function(d, vars) {
  for (m in vars) {
    x  <- d[[m]]
    cc <- x - ave(x, d$event_code, FUN = function(v) mean(v, na.rm = TRUE))
    B  <- ave(cc, d$pid, FUN = function(v) mean(v, na.rm = TRUE))
    W  <- cc - B
    W[is.na(W)] <- 0
    Bp <- B[!duplicated(d$pid)]
    d[[paste0(m, "_W")]] <- W
    d[[paste0(m, "_B")]] <- B - mean(Bp, na.rm = TRUE)
  }
  d
}

# ---- linear mixed models (lme4 + lmerTest + clubSandwich) ---------------------------------
PKGS <- c("lme4", "lmerTest", "clubSandwich")
.miss <- PKGS[!vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
if (length(.miss)) {
  message("Installing missing packages: ", paste(.miss, collapse = ", "))
  install.packages(.miss, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(lme4)
  library(lmerTest)
  library(clubSandwich)
})
LMM_CTRL <- lmerControl(calc.derivs = FALSE)

# random-intercept model; REML; t tests with Satterthwaite degrees of freedom
fit_lmm <- function(formula, data) {
  lmerTest::lmer(formula, data = data, REML = TRUE, control = LMM_CTRL)
}
# CR2 cluster-robust covariance of the fixed effects, clustered by participant
vcov_cr2 <- function(fit, cluster) {
  V <- as.matrix(clubSandwich::vcovCR(fit, cluster = cluster, type = "CR2"))
  dimnames(V) <- list(names(lme4::fixef(fit)), names(lme4::fixef(fit)))
  V
}
# linear combination of fixed effects, e.g. w = c(trait_W = 1, AVxtrait_W = 1)
# returns estimate, SE, 95% CI and p (Satterthwaite t), and p from the CR2 SE (normal reference)
lin <- function(fit, w, V = NULL) {
  b <- lme4::fixef(fit)
  if (!all(names(w) %in% names(b))) stop("unknown term(s): ", paste(setdiff(names(w), names(b)), collapse = ", "))
  L <- setNames(numeric(length(b)), names(b)); L[names(w)] <- w
  r <- lmerTest::contest1D(fit, L)
  est <- r[["Estimate"]]; se <- r[["Std. Error"]]; df <- r[["df"]]
  q <- qt(.975, df)
  p_rob <- NA_real_
  if (!is.null(V)) p_rob <- 2 * pnorm(-abs(est / sqrt(drop(t(L) %*% V %*% L))))
  list(est = est, se = se, lo = est - q * se, hi = est + q * se, df = df, p = r[["Pr(>|t|)"]], p_robust = p_rob)
}
icc <- function(fit) {
  vc <- as.data.frame(lme4::VarCorr(fit))
  vc$vcov[vc$grp == "pid"] / sum(vc$vcov)
}

# ---- person-level (moderated) parallel mediation for a binary X ----------------------------
# a = group difference in mediator means; b paths from OLS Y ~ X + M (+ X:M);
# indirect = a * (b0 + d) (natural indirect effect for X = 1); direct = pure natural direct effect
med <- function(P, x, mv, y, moderated = TRUE) {
  X <- P[[x]]; Y <- P[[y]]; M <- as.matrix(P[, mv, drop = FALSE])
  a  <- colMeans(M[X == 1, , drop = FALSE]) - colMeans(M[X == 0, , drop = FALSE])
  m0 <- colMeans(M[X == 0, , drop = FALSE])
  Z <- cbind(1, X, M)
  if (moderated) Z <- cbind(Z, X * M)
  cf <- lm.fit(Z, Y)$coefficients
  k <- length(mv)
  b0 <- cf[3:(2 + k)]
  dd <- if (moderated) cf[(3 + k):(2 + 2 * k)] else rep(0, k)
  ind <- a * (b0 + dd)
  pnde <- cf[2] + sum(dd * m0)
  tot <- mean(Y[X == 1]) - mean(Y[X == 0])
  unname(c(a, b0, b0 + dd, ind, sum(ind), pnde, tot))
}
med_names <- function(mv) c(paste0("a_", mv), paste0("b_", mv, "_X0"), paste0("b_", mv, "_X1"),
                            paste0("ind_", mv), "ind_total", "direct", "total")
# stratified resample of row indices (within the levels of a binary variable)
strat_idx <- function(x) {
  unlist(lapply(c(0, 1), function(v) { g <- which(x == v); g[sample.int(length(g), length(g), replace = TRUE)] }))
}
boot_med <- function(P, x, mv, y, moderated = TRUE, nboot = NBOOT) {
  est <- med(P, x, mv, y, moderated)
  B <- t(vapply(seq_len(nboot), function(i) med(P[strat_idx(P[[x]]), , drop = FALSE], x, mv, y, moderated),
                numeric(length(est))))
  data.frame(term = med_names(mv), est = est,
             lo = apply(B, 2, quantile, probs = .025, names = FALSE),
             hi = apply(B, 2, quantile, probs = .975, names = FALSE), stringsAsFactors = FALSE)
}

# ---- formatting: round once from full precision, half away from zero -----------------------------
MINUS <- "\u2212"
rnd_half_up <- function(x, k) {
  z <- round(abs(x) * 10^k, 9)            # removes binary representation error (e.g. 0.285 -> 28.5)
  sign(x) * floor(z + 0.5) / 10^k
}
fmt <- function(x, k = 2, small = FALSE) {
  # small = TRUE (CI bounds): a nonzero value that would round to zero keeps one or two more decimals
  if (length(x) != 1) return(vapply(x, fmt, character(1), k = k, small = small))
  if (is.null(x) || is.na(x)) return("\u2014")
  q <- rnd_half_up(x, k); kk <- k
  if (q == 0 && small && x != 0) {
    kk <- k + 1; q <- rnd_half_up(x, kk)
    if (q == 0) { kk <- k + 2; q <- rnd_half_up(x, kk) }
  }
  if (q == 0) return(formatC(0, format = "f", digits = kk))
  s <- formatC(abs(q), format = "f", digits = kk)
  if (q < 0) paste0(MINUS, s) else s
}
fmt_ci <- function(est, lo, hi, k = 2) sprintf("%s [%s, %s]", fmt(est, k), fmt(lo, k, TRUE), fmt(hi, k, TRUE))
fmt_p <- function(p) {
  if (length(p) != 1) return(vapply(p, fmt_p, character(1)))
  if (is.na(p)) return("\u2014")
  if (p < .001) return("< .001")
  s <- fmt(p, 3)
  if (s == "1.000") return("1.00")
  sub("^0", "", s)
}
fmt_msd <- function(m, s, k = 2) sprintf("%s (%s)", fmt(m, k), fmt(s, k))
