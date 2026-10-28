
# Main Analysis

## Overview
Subfolders contain scripts for:

1) Cohort extraction (SQL) - run after connecting to the MIMIC-IV database via PhysioNet and creating the derived concept tables, such as Sepsis 3.
2) Preprocessing and simulation study (R) - must be run after the cohort extraction scripts as they reference the sepsis_final table.

