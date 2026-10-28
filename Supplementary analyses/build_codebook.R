#!/usr/bin/env Rscript
#
# build_codebook.R
#
# Generates a data codebook (data dictionary) for the MIMIC-IV-derived sepsis
# 30-day mortality cohort used in:
#   Darr E, Shaffer JG. Comparing performance measure behavior across
#   statistical and machine learning classifiers for sepsis mortality
#   prediction: a simulation study.
#
# Usage:
#   Rscript build_codebook.R path/to/mymimic_sepsisfinal.csv codebook.xlsx
#
# Requires: dplyr, openxlsx
#   install.packages(c("dplyr", "openxlsx"))

suppressPackageStartupMessages({
  library(dplyr)
})

# ---------------------------------------------------------------------------
# Variable metadata: description, role in analysis, units, data type, coding
# ---------------------------------------------------------------------------
variable_metadata <- list(
  subject_id                 = list(desc = "Unique patient identifier",                                  role = "Identifier",               units = NA, vtype = "Integer",             coding = NA),
  stay_id                    = list(desc = "Unique ICU stay identifier",                                  role = "Identifier",               units = NA, vtype = "Integer",             coding = NA),
  age_mean                   = list(desc = "Patient age at ICU admission",                                role = "Predictor",                units = "years",        vtype = "Continuous",          coding = NA),
  height_mean                = list(desc = "Patient height",                                              role = "Excluded (>10% missing)",   units = "cm",           vtype = "Continuous",          coding = NA),
  los_icu                    = list(desc = "ICU length of stay",                                          role = "Excluded (data leakage)",   units = "days",         vtype = "Continuous",          coding = NA),
  sofa_score                 = list(desc = "Sequential Organ Failure Assessment score, first ICU day",    role = "Predictor",                units = "score (0-24)", vtype = "Continuous (count)",  coding = NA),
  gender                     = list(desc = "Patient sex",                                                  role = "Predictor",                units = NA, vtype = "Binary",              coding = "M = male, F = female"),
  los_hospital               = list(desc = "Total hospital length of stay",                                role = "Excluded (data leakage)",   units = "days",         vtype = "Continuous",          coding = NA),
  aniongap_max               = list(desc = "Anion gap, maximum value, first ICU day",                     role = "Predictor",                units = "mEq/L",        vtype = "Continuous",          coding = NA),
  aniongap_min               = list(desc = "Anion gap, minimum value, first ICU day",                     role = "Predictor",                units = "mEq/L",        vtype = "Continuous",          coding = NA),
  bicarbonate_max            = list(desc = "Bicarbonate, maximum value, first ICU day",                   role = "Predictor",                units = "mEq/L",        vtype = "Continuous",          coding = NA),
  bicarbonate_min            = list(desc = "Bicarbonate, minimum value, first ICU day",                   role = "Predictor",                units = "mEq/L",        vtype = "Continuous",          coding = NA),
  inr_max                    = list(desc = "International normalized ratio, maximum value, first ICU day",role = "Predictor",                units = "ratio",        vtype = "Continuous",          coding = NA),
  sodium_max                 = list(desc = "Sodium, maximum value, first ICU day",                        role = "Predictor",                units = "mEq/L",        vtype = "Continuous",          coding = NA),
  sodium_min                 = list(desc = "Sodium, minimum value, first ICU day",                        role = "Predictor",                units = "mEq/L",        vtype = "Continuous",          coding = NA),
  chloride_max               = list(desc = "Chloride, maximum value, first ICU day",                      role = "Predictor",                units = "mEq/L",        vtype = "Continuous",          coding = NA),
  chloride_min               = list(desc = "Chloride, minimum value, first ICU day",                      role = "Predictor",                units = "mEq/L",        vtype = "Continuous",          coding = NA),
  bun_max                    = list(desc = "Blood urea nitrogen, maximum value, first ICU day",           role = "Predictor",                units = "mg/dL",        vtype = "Continuous",          coding = NA),
  bun_min                    = list(desc = "Blood urea nitrogen, minimum value, first ICU day",           role = "Predictor",                units = "mg/dL",        vtype = "Continuous",          coding = NA),
  wbc_max                    = list(desc = "White blood cell count, maximum value, first ICU day",        role = "Predictor",                units = "10^3/uL",      vtype = "Continuous",          coding = NA),
  wbc_min                    = list(desc = "White blood cell count, minimum value, first ICU day",        role = "Predictor",                units = "10^3/uL",      vtype = "Continuous",          coding = NA),
  hematocrit_max             = list(desc = "Hematocrit, maximum value, first ICU day",                    role = "Predictor",                units = "%",            vtype = "Continuous",          coding = NA),
  hematocrit_min             = list(desc = "Hematocrit, minimum value, first ICU day",                    role = "Predictor",                units = "%",            vtype = "Continuous",          coding = NA),
  hemoglobin_max             = list(desc = "Hemoglobin, maximum value, first ICU day",                    role = "Predictor",                units = "g/dL",         vtype = "Continuous",          coding = NA),
  hemoglobin_min             = list(desc = "Hemoglobin, minimum value, first ICU day",                    role = "Predictor",                units = "g/dL",         vtype = "Continuous",          coding = NA),
  creatinine_max             = list(desc = "Creatinine, maximum value, first ICU day",                    role = "Predictor",                units = "mg/dL",        vtype = "Continuous",          coding = NA),
  creatinine_min             = list(desc = "Creatinine, minimum value, first ICU day",                    role = "Predictor",                units = "mg/dL",        vtype = "Continuous",          coding = NA),
  spo2_max                   = list(desc = "Peripheral oxygen saturation, maximum, first ICU day",        role = "Predictor",                units = "%",            vtype = "Continuous",          coding = NA),
  spo2_min                   = list(desc = "Peripheral oxygen saturation, minimum, first ICU day",        role = "Predictor",                units = "%",            vtype = "Continuous",          coding = NA),
  spo2_mean                  = list(desc = "Peripheral oxygen saturation, mean, first ICU day",           role = "Predictor",                units = "%",            vtype = "Continuous",          coding = NA),
  sbp_max                    = list(desc = "Systolic blood pressure, maximum, first ICU day",             role = "Predictor",                units = "mmHg",         vtype = "Continuous",          coding = NA),
  sbp_min                    = list(desc = "Systolic blood pressure, minimum, first ICU day",             role = "Predictor",                units = "mmHg",         vtype = "Continuous",          coding = NA),
  sbp_mean                   = list(desc = "Systolic blood pressure, mean, first ICU day",                role = "Predictor",                units = "mmHg",         vtype = "Continuous",          coding = NA),
  dbp_max                    = list(desc = "Diastolic blood pressure, maximum, first ICU day",            role = "Predictor",                units = "mmHg",         vtype = "Continuous",          coding = NA),
  dbp_min                    = list(desc = "Diastolic blood pressure, minimum, first ICU day",            role = "Predictor",                units = "mmHg",         vtype = "Continuous",          coding = NA),
  dbp_mean                   = list(desc = "Diastolic blood pressure, mean, first ICU day",               role = "Predictor",                units = "mmHg",         vtype = "Continuous",          coding = NA),
  heart_rate_max             = list(desc = "Heart rate, maximum, first ICU day",                          role = "Predictor",                units = "bpm",          vtype = "Continuous",          coding = NA),
  resp_rate_max              = list(desc = "Respiratory rate, maximum, first ICU day",                    role = "Predictor",                units = "breaths/min",  vtype = "Continuous",          coding = NA),
  temperature_max            = list(desc = "Body temperature, maximum, first ICU day",                    role = "Predictor",                units = "\u00b0C",      vtype = "Continuous",          coding = NA),
  urine_max                  = list(desc = "Urine output, maximum, first ICU day",                        role = "Predictor",                units = "mL",           vtype = "Continuous",          coding = NA),
  metastatic_solid_tumor_max = list(desc = "Metastatic solid tumor comorbidity indicator",                role = "Predictor",                units = NA, vtype = "Binary",              coding = "0 = absent, 1 = present"),
  charlson_score_max         = list(desc = "Charlson Comorbidity Index",                                  role = "Predictor",                units = "score",        vtype = "Continuous (count)",  coding = NA),
  sapsii                     = list(desc = "Simplified Acute Physiology Score II",                        role = "Predictor",                units = "score",        vtype = "Continuous (count)",  coding = NA),
  sapsii_prob_max            = list(desc = "SAPS II predicted mortality probability",                     role = "Predictor",                units = "probability (0-1)", vtype = "Continuous",     coding = NA),
  vent_status                = list(desc = "Mechanical ventilation status, first ICU day",                role = "Predictor",                units = NA, vtype = "Categorical",         coding = "0 = no ventilation, 3 = non-invasive, 4 = invasive"),
  sus_anti_period            = list(desc = "Suspected infection / antibiotic suspicion period",           role = "Predictor",                units = "days",         vtype = "Continuous (count)",  coding = NA),
  cul_anti_period            = list(desc = "Culture-to-antibiotic period",                                role = "Predictor",                units = "days",         vtype = "Continuous (count)",  coding = NA),
  antibiotic_num             = list(desc = "Number of distinct antibiotics administered",                 role = "Predictor",                units = "count",        vtype = "Continuous (count)",  coding = NA),
  specimen_count              = list(desc = "Number of culture specimens collected",                       role = "Predictor",                units = "count",        vtype = "Continuous (count)",  coding = NA),
  positive_culture           = list(desc = "Positive microbiological culture indicator",                  role = "Predictor",                units = NA, vtype = "Binary",              coding = "0 = negative, 1 = positive"),
  crp_max                    = list(desc = "C-reactive protein, maximum value, first ICU day",            role = "Excluded (>10% missing)",   units = "mg/L",         vtype = "Continuous",          coding = NA),
  death_within_30_days       = list(desc = "30-day all-cause mortality (outcome)",                        role = "Outcome",                  units = NA, vtype = "Binary",              coding = "0 = alive at 30 days, 1 = deceased within 30 days")
)

#' Build a codebook data frame from the raw cohort CSV
#'
#' @param csv_path path to mymimic_sepsisfinal.csv
#' @return data.frame with one row per variable
build_codebook <- function(csv_path) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)

  rows <- lapply(names(df), function(col) {
    meta <- variable_metadata[[col]]
    if (is.null(meta)) {
      meta <- list(desc = "", role = "", units = NA, vtype = "", coding = NA)
    }

    x <- df[[col]]
    n_nonmissing <- sum(!is.na(x))
    miss_pct <- mean(is.na(x)) * 100

    if (startsWith(meta$vtype, "Continuous")) {
      summary_label <- "Mean (SD)"
      summary_stat <- sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
    } else if (meta$vtype %in% c("Binary", "Categorical")) {
      summary_label <- "n (%) per category"
      tab <- sort(table(x, useNA = "no"), decreasing = TRUE)
      parts <- sprintf("%s: %d (%.1f%%)", names(tab), as.integer(tab),
                        as.integer(tab) / n_nonmissing * 100)
      summary_stat <- paste(parts, collapse = "; ")
    } else {
      summary_label <- ""
      summary_stat <- ""
    }

    data.frame(
      `Variable Name` = col,
      Description = meta$desc,
      `Role in Analysis` = meta$role,
      `Data Type` = meta$vtype,
      Units = ifelse(is.na(meta$units), "", meta$units),
      Coding = ifelse(is.na(meta$coding), "", meta$coding),
      `Missing (%)` = round(miss_pct, 2),
      `Summary Statistic Type` = summary_label,
      `Summary Statistic` = summary_stat,
      `N Non-Missing` = n_nonmissing,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

notes_text <- c(
  "Notes on Preprocessing and Feature Exclusions",
  "",
  "Excluded due to >10% missingness: height_mean (35.1% missing), crp_max (72.8% missing).",
  "Excluded due to data leakage: los_icu, los_hospital (not available at the point of early clinical prediction).",
  "Identifiers (not used as predictors): subject_id, stay_id.",
  "Outcome variable: death_within_30_days.",
  "",
  "Categorical encoding: gender was coded as a binary indicator (M = 1). vent_status was recoded",
  "into two binary indicators (invasive ventilation, non-invasive ventilation) with \"no ventilation\"",
  "as the reference category, consistent with Appendix Table 1d.",
  "",
  "Missing continuous values were imputed with the training-set median; missing categorical values",
  "were encoded as a separate \"missing\" indicator category, per the Methods section of the manuscript.",
  "",
  "Of the 45 candidate predictors remaining after removing identifiers, leakage variables, and",
  "high-missingness variables, 37 met the univariate screening threshold (p < 0.50) and were",
  "retained in the final classification models, per the manuscript's Methods section. The exact",
  "final 37-variable list is determined by the univariate screening step in the analysis code and",
  "is not re-derived here."
)

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  csv_path <- if (length(args) >= 1) args[1] else "mymimic_sepsisfinal.csv"
  out_path <- if (length(args) >= 2) args[2] else "codebook.xlsx"

  codebook <- build_codebook(csv_path)

  if (grepl("\\.csv$", out_path, ignore.case = TRUE)) {
    write.csv(codebook, out_path, row.names = FALSE)
  } else {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required to write .xlsx output. ",
           "Install it with install.packages('openxlsx'), or pass a .csv output path instead.")
    }
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Codebook")
    openxlsx::writeData(wb, "Codebook", codebook)
    openxlsx::addWorksheet(wb, "Notes")
    openxlsx::writeData(wb, "Notes", data.frame(Notes = notes_text), colNames = FALSE)
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  }

  cat(sprintf("Wrote codebook with %d variables to %s\n", nrow(codebook), out_path))
}

if (sys.nframe() == 0) {
  main()
}
