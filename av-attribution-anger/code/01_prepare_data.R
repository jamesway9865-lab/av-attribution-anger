# =============================================================================
# Step 1: build the four analysis data files from the source data.
#
# Input : data/source/study1_source.csv, data/source/study2_source.csv
# Output: data/processed/study1_long.csv, study1_person.csv, study2_long.csv, study2_person.csv
# Variables are described in docs/codebook.md.
# Run   : Rscript code/01_prepare_data.R
# =============================================================================
if (!exists("ROOT")) {
  a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  ROOT <- if (nzchar(Sys.getenv("REPO_ROOT"))) Sys.getenv("REPO_ROOT") else
    if (length(f)) normalizePath(file.path(dirname(f[1]), ".."), winslash = "/") else getwd()
}
source(file.path(ROOT, "code", "functions.R"), encoding = "UTF-8")
dir.create(DIR_DAT, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------ Study 1
s1 <- rd_csv(file.path(DIR_SRC, "study1_source.csv"))
EVENTS1 <- data.frame(
  code = c("CA01", "CA02", "CA03", "CA041", "CA042", "CS01", "CS02", "CS03", "WA01", "WA02"),
  no   = c("1", "2", "3", "4a", "4b", "5", "6", "7", "8", "9"),
  label = c("Slow vehicle ahead", "Sudden braking ahead", "Stopping behind an obstacle",
            "Rear-end collision (simple road)", "Rear-end collision (complex road)", "High beams at night",
            "Accelerating while being overtaken", "Cutting into a queue", "Left turn without yielding",
            "Taking a parking space"), stringsAsFactors = FALSE)
# class of each candidate cause (a1..a5 = 1st..5th cause listed for the scenario; see docs/codebook.md)
COMP <- list(CA02 = "a1", CA03 = "a1", CA041 = "a1", CA042 = "a1", CS01 = "a1")
CHAR <- list(CA01 = "a1", CA02 = "a2", CS02 = "a1", CS03 = "a1", WA01 = c("a1", "a2"), WA02 = c("a1", "a2"))
MOM  <- list(CA01 = "a2", CA02 = "a3", CA03 = character(0), CA041 = "a2", CA042 = "a2", CS01 = "a2",
             CS02 = c("a2", "a3"), CS03 = "a2", WA01 = c("a3", "a4"), WA02 = c("a3", "a4"))
SIT  <- list(CA01 = "a3", CA02 = "a4", CA03 = c("a2", "a3"), CA041 = c("a3", "a4"), CA042 = c("a3", "a4"),
             CS01 = "a3", CS02 = "a4", CS03 = "a3", WA01 = "a5", WA02 = "a5")
opt <- function(lst, code) if (is.null(lst[[code]])) character(0) else lst[[code]]

av1 <- as.integer(s1$HVorAV == 2)
S1L <- do.call(rbind, lapply(seq_len(nrow(EVENTS1)), function(i) {
  code <- EVENTS1$code[i]
  m <- function(opts) rmean(s1, if (length(opts)) paste0("BEHAVaggressive", code, "_", opts) else character(0))
  data.frame(pid = s1$id, vehicle = ifelse(av1 == 1, "AV", "HV"), AV = av1,
             event_code = code, event_no = EVENTS1$no[i], event = EVENTS1$label[i],
             trait_comp = m(opt(COMP, code)), trait_char = m(opt(CHAR, code)),
             trait = m(c(opt(COMP, code), opt(CHAR, code))), momentary = m(opt(MOM, code)),
             situational = m(opt(SIT, code)),
             anger = s1[[paste0("BEHAVaggressive", code, "_angry")]],
             blame = s1[[paste0("BEHAVaggressive", code, "_blame")]], stringsAsFactors = FALSE)
}))
wr_csv(S1L, file.path(DIR_DAT, "study1_long.csv"))

person_means <- function(L, vars) {
  out <- data.frame(pid = sort(unique(L$pid)))
  for (v in vars) out[[paste0("mean_", v)]] <- as.numeric(gmean(L[[v]], L$pid)[as.character(out$pid)])
  out
}
P1 <- data.frame(pid = s1$id, vehicle = ifelse(av1 == 1, "AV", "HV"), AV = av1,
                 female = as.integer(s1$gender == 2), age = s1$age, driving_years = s1$drivingyear,
                 mileage_10k_km = s1$drivinglengthall, drivertype_raw = s1$drivertype, education_raw = s1$education,
                 safety_current = s1$TRAIT_CA1_d, safety_expected = s1$TRAIT_CA1_p,
                 ability_current = rmean(s1, paste0("TRAIT_CA", 4:6, "_d")),
                 ability_expected = rmean(s1, paste0("TRAIT_CA", 4:6, "_p")), stringsAsFactors = FALSE)
P1$safety_gap  <- P1$safety_expected - P1$safety_current
P1$ability_gap <- P1$ability_expected - P1$ability_current
P1 <- merge(P1, person_means(S1L, c("trait_comp", "trait_char", "trait", "momentary", "situational", "anger", "blame")),
            by = "pid", sort = FALSE)
P1 <- P1[match(s1$id, P1$pid), ]
wr_csv(P1, file.path(DIR_DAT, "study1_person.csv"))

# ------------------------------------------------------------------ Study 2
s2 <- rd_csv(file.path(DIR_SRC, "study2_source.csv"))
GRP <- c("0" = "control", "1" = "ability", "2" = "rule_compliance")
EVENTS2 <- data.frame(code = paste0("E", 1:5), no = as.character(1:5),
                      label = c("Stopping behind an obstacle", "Stopping in a right-turn lane", "Sudden deceleration ahead",
                                "Accelerating while being overtaken", "Cutting in at close range"), stringsAsFactors = FALSE)
pid2 <- seq_len(nrow(s2))
S2L <- do.call(rbind, lapply(seq_len(nrow(EVENTS2)), function(i) {
  code <- EVENTS2$code[i]; withchar <- code %in% c("E4", "E5")
  data.frame(pid = pid2, group = unname(GRP[as.character(s2$condition)]), group_code = s2$condition,
             event_code = code, event_no = EVENTS2$no[i], event = EVENTS2$label[i],
             trait_comp = s2[[paste0(code, "NSC")]],
             trait_char = if (withchar) rmean(s2, paste0(code, c("NSS", "NSB"))) else NA_real_,
             trait = rmean(s2, paste0(code, if (withchar) c("NSC", "NSS", "NSB") else "NSC")),
             momentary = s2[[paste0(code, "NUC")]], situational = s2[[paste0(code, "W")]],
             anger = s2[[paste0(code, "emo")]], punishment = s2[[paste0(code, "beh")]], stringsAsFactors = FALSE)
}))
wr_csv(S2L, file.path(DIR_DAT, "study2_long.csv"))

ctrl <- s2$condition == 0
P2 <- data.frame(pid = pid2, group = unname(GRP[as.character(s2$condition)]), group_code = s2$condition,
                 female = as.integer(s2$gender == 2), age = s2$age, driving_years = s2$drivingyear,
                 driving_mileage_raw = s2$drivingmile, av_knowledge = s2$knowledge, drivertype_raw = s2$drivertype,
                 ability_pre = s2$C1, ability_post = ifelse(ctrl, NA, s2$C2),          # control group: no post-test
                 normativity_pre = s2$B1, normativity_post = ifelse(ctrl, NA, s2$B2), stringsAsFactors = FALSE)
P2 <- merge(P2, person_means(S2L, c("trait_comp", "trait_char", "trait", "momentary", "situational", "anger", "punishment")),
            by = "pid", sort = TRUE)
wr_csv(P2, file.path(DIR_DAT, "study2_person.csv"))

cat(sprintf("study1_long %d x %d | study1_person %d x %d | study2_long %d x %d | study2_person %d x %d\n",
            nrow(S1L), ncol(S1L), nrow(P1), ncol(P1), nrow(S2L), ncol(S2L), nrow(P2), ncol(P2)))
