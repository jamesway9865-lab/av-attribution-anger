# =============================================================================
# Step 0 (authors only): create the shareable source data from the original questionnaire exports.
#
# The original exports (data/raw/Data_E1E3E5.csv for Study 1, data/raw/Data_E4E6.csv for Study 2) are
# NOT part of the public repository: they also contain measures from a larger project that are not
# reported in this paper (yielding intentions, moral dilemmas, further trait ratings) and platform
# respondent IDs. This script keeps only the variables used in the paper, drops the platform IDs and
# writes data/source/study1_source.csv and data/source/study2_source.csv. Row order is preserved, so
# participant numbers in the processed data are identical to those used in all analyses.
#
# Run (authors only, with the raw files in data/raw/ or in the folder given by the environment variable RAW_DIR):
#   Rscript code/00_make_source_data.R
# =============================================================================
if (!exists("ROOT")) {
  a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  ROOT <- if (nzchar(Sys.getenv("REPO_ROOT"))) Sys.getenv("REPO_ROOT") else
    if (length(f)) normalizePath(file.path(dirname(f[1]), ".."), winslash = "/") else getwd()
}
source(file.path(ROOT, "code", "functions.R"), encoding = "UTF-8")
DIR_RAW <- if (nzchar(Sys.getenv("RAW_DIR"))) Sys.getenv("RAW_DIR") else file.path(ROOT, "data", "raw")
dir.create(DIR_SRC, recursive = TRUE, showWarnings = FALSE)

# The raw exports are UTF-8 with a byte-order mark; some free-text answers to measures not used here
# contain invalid bytes, so the files are read without re-encoding.
rd_raw <- function(f) {
  d <- read.csv(file.path(DIR_RAW, f), encoding = "UTF-8", stringsAsFactors = FALSE, check.names = FALSE,
                na.strings = c("", " ", "NA"))
  names(d)[1] <- sub("^[^A-Za-z]+", "", names(d)[1])      # strip the byte-order mark from the first name
  d
}

# ---------------------------------------------------------------- Study 1 (N = 1,202)
s1 <- rd_raw("Data_E1E3E5.csv")
keep1 <- c("id", "HVorAV", "drivertype", "gender", "age", "education", "drivingyear", "drivinglengthall",
           grep("^BEHAVaggressive", names(s1), value = TRUE),            # 10 error conditions
           "TRAIT_CA1_d", "TRAIT_CA1_p",                                  # overall driving safety
           paste0("TRAIT_CA", rep(4:6, each = 2), c("_d", "_p")))         # three driving-ability items
stopifnot(all(keep1 %in% names(s1)))
wr_csv(s1[, keep1], file.path(DIR_SRC, "study1_source.csv"))

# ---------------------------------------------------------------- Study 2 (N = 723 retained)
s2 <- rd_raw("Data_E4E6.csv")
# the condition column (Chinese header) follows B1 and precedes C12
j <- which(names(s2) == "B1") + 1
stopifnot(names(s2)[j + 1] == "C12")
names(s2)[j] <- "condition"
keep2 <- c("condition", "drivertype", "gender", "age", "education", "drivingyear", "drivingmile", "knowledge",
           "C11", "C21", "C31", "C1", "B11", "B21", "B31", "B1",                  # pre-information ratings
           "C12", "C22", "C32", "C2", "B12", "B22", "B32", "B2",                  # post-information ratings
           grep("^E[1-5]", names(s2), value = TRUE))                              # five AV errors
stopifnot(all(keep2 %in% names(s2)))
wr_csv(s2[, keep2], file.path(DIR_SRC, "study2_source.csv"))                   # ID1/ID2 (platform IDs) dropped

cat(sprintf("study1_source %d x %d | study2_source %d x %d\n", nrow(s1), length(keep1), nrow(s2), length(keep2)))
