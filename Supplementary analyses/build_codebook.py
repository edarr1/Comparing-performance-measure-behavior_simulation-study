"""
build_codebook.py

Generates a data codebook (data dictionary) for the MIMIC-IV-derived sepsis
30-day mortality cohort used in:
  Darr E, Shaffer JG. Comparing performance measure behavior across
  statistical and machine learning classifiers for sepsis mortality
  prediction: a simulation study.

Usage:
    python build_codebook.py path/to/mymimic_sepsisfinal.csv codebook.xlsx

Requires: pandas, openpyxl
    pip install pandas openpyxl
"""

import sys
import pandas as pd

# ---------------------------------------------------------------------------
# Variable metadata: description, role in analysis, units, data type, coding
# ---------------------------------------------------------------------------
VARIABLE_METADATA = {
    "subject_id": ("Unique patient identifier", "Identifier", None, "Integer", None),
    "stay_id": ("Unique ICU stay identifier", "Identifier", None, "Integer", None),
    "age_mean": ("Patient age at ICU admission", "Predictor", "years", "Continuous", None),
    "height_mean": ("Patient height", "Excluded (>10% missing)", "cm", "Continuous", None),
    "los_icu": ("ICU length of stay", "Excluded (data leakage)", "days", "Continuous", None),
    "sofa_score": ("Sequential Organ Failure Assessment score, first ICU day", "Predictor", "score (0-24)", "Continuous (count)", None),
    "gender": ("Patient sex", "Predictor", None, "Binary", "M = male, F = female"),
    "los_hospital": ("Total hospital length of stay", "Excluded (data leakage)", "days", "Continuous", None),
    "aniongap_max": ("Anion gap, maximum value, first ICU day", "Predictor", "mEq/L", "Continuous", None),
    "aniongap_min": ("Anion gap, minimum value, first ICU day", "Predictor", "mEq/L", "Continuous", None),
    "bicarbonate_max": ("Bicarbonate, maximum value, first ICU day", "Predictor", "mEq/L", "Continuous", None),
    "bicarbonate_min": ("Bicarbonate, minimum value, first ICU day", "Predictor", "mEq/L", "Continuous", None),
    "inr_max": ("International normalized ratio, maximum value, first ICU day", "Predictor", "ratio", "Continuous", None),
    "sodium_max": ("Sodium, maximum value, first ICU day", "Predictor", "mEq/L", "Continuous", None),
    "sodium_min": ("Sodium, minimum value, first ICU day", "Predictor", "mEq/L", "Continuous", None),
    "chloride_max": ("Chloride, maximum value, first ICU day", "Predictor", "mEq/L", "Continuous", None),
    "chloride_min": ("Chloride, minimum value, first ICU day", "Predictor", "mEq/L", "Continuous", None),
    "bun_max": ("Blood urea nitrogen, maximum value, first ICU day", "Predictor", "mg/dL", "Continuous", None),
    "bun_min": ("Blood urea nitrogen, minimum value, first ICU day", "Predictor", "mg/dL", "Continuous", None),
    "wbc_max": ("White blood cell count, maximum value, first ICU day", "Predictor", "10^3/uL", "Continuous", None),
    "wbc_min": ("White blood cell count, minimum value, first ICU day", "Predictor", "10^3/uL", "Continuous", None),
    "hematocrit_max": ("Hematocrit, maximum value, first ICU day", "Predictor", "%", "Continuous", None),
    "hematocrit_min": ("Hematocrit, minimum value, first ICU day", "Predictor", "%", "Continuous", None),
    "hemoglobin_max": ("Hemoglobin, maximum value, first ICU day", "Predictor", "g/dL", "Continuous", None),
    "hemoglobin_min": ("Hemoglobin, minimum value, first ICU day", "Predictor", "g/dL", "Continuous", None),
    "creatinine_max": ("Creatinine, maximum value, first ICU day", "Predictor", "mg/dL", "Continuous", None),
    "creatinine_min": ("Creatinine, minimum value, first ICU day", "Predictor", "mg/dL", "Continuous", None),
    "spo2_max": ("Peripheral oxygen saturation, maximum, first ICU day", "Predictor", "%", "Continuous", None),
    "spo2_min": ("Peripheral oxygen saturation, minimum, first ICU day", "Predictor", "%", "Continuous", None),
    "spo2_mean": ("Peripheral oxygen saturation, mean, first ICU day", "Predictor", "%", "Continuous", None),
    "sbp_max": ("Systolic blood pressure, maximum, first ICU day", "Predictor", "mmHg", "Continuous", None),
    "sbp_min": ("Systolic blood pressure, minimum, first ICU day", "Predictor", "mmHg", "Continuous", None),
    "sbp_mean": ("Systolic blood pressure, mean, first ICU day", "Predictor", "mmHg", "Continuous", None),
    "dbp_max": ("Diastolic blood pressure, maximum, first ICU day", "Predictor", "mmHg", "Continuous", None),
    "dbp_min": ("Diastolic blood pressure, minimum, first ICU day", "Predictor", "mmHg", "Continuous", None),
    "dbp_mean": ("Diastolic blood pressure, mean, first ICU day", "Predictor", "mmHg", "Continuous", None),
    "heart_rate_max": ("Heart rate, maximum, first ICU day", "Predictor", "bpm", "Continuous", None),
    "resp_rate_max": ("Respiratory rate, maximum, first ICU day", "Predictor", "breaths/min", "Continuous", None),
    "temperature_max": ("Body temperature, maximum, first ICU day", "Predictor", "\u00b0C", "Continuous", None),
    "urine_max": ("Urine output, maximum, first ICU day", "Predictor", "mL", "Continuous", None),
    "metastatic_solid_tumor_max": ("Metastatic solid tumor comorbidity indicator", "Predictor", None, "Binary", "0 = absent, 1 = present"),
    "charlson_score_max": ("Charlson Comorbidity Index", "Predictor", "score", "Continuous (count)", None),
    "sapsii": ("Simplified Acute Physiology Score II", "Predictor", "score", "Continuous (count)", None),
    "sapsii_prob_max": ("SAPS II predicted mortality probability", "Predictor", "probability (0-1)", "Continuous", None),
    "vent_status": ("Mechanical ventilation status, first ICU day", "Predictor", None, "Categorical", "0 = no ventilation, 3 = non-invasive, 4 = invasive"),
    "sus_anti_period": ("Suspected infection / antibiotic suspicion period", "Predictor", "days", "Continuous (count)", None),
    "cul_anti_period": ("Culture-to-antibiotic period", "Predictor", "days", "Continuous (count)", None),
    "antibiotic_num": ("Number of distinct antibiotics administered", "Predictor", "count", "Continuous (count)", None),
    "specimen_count": ("Number of culture specimens collected", "Predictor", "count", "Continuous (count)", None),
    "positive_culture": ("Positive microbiological culture indicator", "Predictor", None, "Binary", "0 = negative, 1 = positive"),
    "crp_max": ("C-reactive protein, maximum value, first ICU day", "Excluded (>10% missing)", "mg/L", "Continuous", None),
    "death_within_30_days": ("30-day all-cause mortality (outcome)", "Outcome", None, "Binary", "0 = alive at 30 days, 1 = deceased within 30 days"),
}


def build_codebook(csv_path: str) -> pd.DataFrame:
    """Read the raw cohort CSV and compute a codebook row per variable."""
    df = pd.read_csv(csv_path)

    rows = []
    for col in df.columns:
        desc, role, units, vtype, coding = VARIABLE_METADATA.get(
            col, ("", "", None, "", None)
        )
        n_nonmissing = df[col].notna().sum()
        miss_pct = df[col].isna().mean() * 100

        if vtype.startswith("Continuous"):
            summary_label = "Mean (SD)"
            summary = f"{df[col].mean():.2f} ({df[col].std():.2f})"
        elif vtype in ("Binary", "Categorical"):
            summary_label = "n (%) per category"
            vc = df[col].value_counts(dropna=True)
            summary = "; ".join(
                f"{k}: {v} ({v / n_nonmissing * 100:.1f}%)" for k, v in vc.items()
            )
        else:
            summary_label = ""
            summary = ""

        rows.append(
            {
                "Variable Name": col,
                "Description": desc,
                "Role in Analysis": role,
                "Data Type": vtype,
                "Units": units or "",
                "Coding": coding or "",
                "Missing (%)": round(miss_pct, 2),
                "Summary Statistic Type": summary_label,
                "Summary Statistic": summary,
                "N Non-Missing": int(n_nonmissing),
            }
        )

    return pd.DataFrame(rows)


NOTES = """Notes on Preprocessing and Feature Exclusions

Excluded due to >10% missingness: height_mean (35.1% missing), crp_max (72.8% missing).
Excluded due to data leakage: los_icu, los_hospital (not available at the point of early clinical prediction).
Identifiers (not used as predictors): subject_id, stay_id.
Outcome variable: death_within_30_days.

Categorical encoding: gender was coded as a binary indicator (M = 1). vent_status was recoded
into two binary indicators (invasive ventilation, non-invasive ventilation) with "no ventilation"
as the reference category, consistent with Appendix Table 1d.

Missing continuous values were imputed with the training-set median; missing categorical values
were encoded as a separate "missing" indicator category, per the Methods section of the manuscript.

Of the 45 candidate predictors remaining after removing identifiers, leakage variables, and
high-missingness variables, 37 met the univariate screening threshold (p < 0.50) and were
retained in the final classification models, per the manuscript's Methods section. The exact
final 37-variable list is determined by the univariate screening step in the analysis code and
is not re-derived here.
"""


def main():
    csv_path = sys.argv[1] if len(sys.argv) > 1 else "mymimic_sepsisfinal.csv"
    out_path = sys.argv[2] if len(sys.argv) > 2 else "codebook.xlsx"

    codebook = build_codebook(csv_path)

    if out_path.lower().endswith(".csv"):
        codebook.to_csv(out_path, index=False)
    else:
        with pd.ExcelWriter(out_path, engine="openpyxl") as writer:
            codebook.to_excel(writer, sheet_name="Codebook", index=False)
            notes_df = pd.DataFrame({"Notes": NOTES.strip("\n").split("\n")})
            notes_df.to_excel(writer, sheet_name="Notes", index=False, header=False)

    print(f"Wrote codebook with {len(codebook)} variables to {out_path}")


if __name__ == "__main__":
    main()
