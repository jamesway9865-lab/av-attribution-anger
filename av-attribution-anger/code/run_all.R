# =============================================================================
# Run the whole analysis in order (from the repository root):
#   Rscript code/run_all.R
# Step 0 runs only if the original exports are present in data/raw/ or in the folder given by the
# environment variable RAW_DIR (authors only).
# A log of the run is written to output/run_log.txt.
# =============================================================================
a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
ROOT <- if (nzchar(Sys.getenv("REPO_ROOT"))) Sys.getenv("REPO_ROOT") else
  if (length(f)) normalizePath(file.path(dirname(f[1]), ".."), winslash = "/") else getwd()
if (!dir.exists(file.path(ROOT, "code"))) stop("Run from the repository root or set REPO_ROOT.")
dir.create(file.path(ROOT, "output"), showWarnings = FALSE)
log_file <- file.path(ROOT, "output", "run_log.txt")
zz <- file(log_file, open = "wt", encoding = "UTF-8")
sink(zz, split = TRUE); sink(zz, type = "message")

steps <- c("01_prepare_data.R", "02_results.R", "03_figures.R", "04_robustness_blame_wording.R", "05_manuscript_tables.R")
raw_dir <- if (nzchar(Sys.getenv("RAW_DIR"))) Sys.getenv("RAW_DIR") else file.path(ROOT, "data", "raw")
if (all(file.exists(file.path(raw_dir, c("Data_E1E3E5.csv", "Data_E4E6.csv"))))) steps <- c("00_make_source_data.R", steps)
t0 <- Sys.time(); cur <- ""
status <- tryCatch({
  for (s in steps) {
    cur <- s
    cat("\n==========", s, "==========\n")
    source(file.path(ROOT, "code", s), local = new.env(), encoding = "UTF-8")
  }
  "ok"
}, error = function(e) {
  cat("\nERROR in", cur, ":", conditionMessage(e), "\n")
  if (!is.null(conditionCall(e))) cat("call:", deparse(conditionCall(e))[1], "\n")
  "error"
})
if (status == "ok") cat(sprintf("\nAll steps finished in %.1f minutes.\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
w <- warnings(); if (length(w)) { cat("\nWarnings:\n"); print(w) }
cat("\n"); print(sessionInfo())
sink(type = "message"); sink(); close(zz)
if (status != "ok" && !interactive()) quit(status = 1)
