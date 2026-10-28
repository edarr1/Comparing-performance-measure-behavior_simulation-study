# Comparing performance measure behavior across statistical and machine learning classifiers for sepsis mortality prediction: a simulation study

## Overview
This repository contains the analysis code for a parametric simulation study characterizing the statistical properties (estiamtion accuracy, precision, and ranking consistency) of ten performance measures (accuracy, balanced accuracy, sensitivity, specificity, PPV, NPV, F1 Score, MCC, ROC-AUC, & Brier score) commonly used to evaluate machine learning models. Further analyses examine precision-recall performance across 25 algorithms and the influence of hyperparameter optimization.

## Repository Details
- Repository name: Comparing-performance-measure-behavior_simulation-study
- Repository link: https://github.com/edarr1/Comparing-performance-measure-behavior_simulation-study
- Authors: Emma Darr, Edmund Agyemang, Samuel Kakraba, Jeffrey Shaffer
- Corresponding Author(s) Contact: skakraba@tulane.edu; jshaffer@tulane.edu
- Research institution: Tulane University, Celia Scott Weatherhead School of Public Health and Tropical Medicine, Department of Biostatistics & Data Science
- Repository contact information: edarr1@tulane.edu

## Repository Contents
```
┌── Main analysis
│   ├── Cohort extraction (SQL)
│   │   ├── sepsis_last_icu.sql 
│   │   ├── sepsis_features.sql 
│   │   ├── sepsis_refine.sql 
│   │   ├── sepsis_final.sql 
│   │   ├── variable description.docx
│   │   └── README.md
│   ├── Preprocessing and simulation study (R)
│   │   ├── load_mimic_database.R
│   │   ├── preprocessing_full.R
│   │   ├── preprocessing_condensed.R
│   │   ├── model_fitting.R 
│   │   ├── dgm_parameters.R 
│   │   ├── simulation_loop.R 
│   │   ├── reference_data.R 
│   │   ├── performance_summary.R 
│   │   ├── ranking_consistency.R
│   │   ├── visualization.R 
│   │   ├── test_performance_tables.R 
│   │   └── README.md
│   └── README.md
├── Supplementary analyses
│   ├── build_codebook.py 
│   ├── build_codebook.R 
│   └── README.md
└── README.md
```

## Data Source
The patient cohort of this study was derived from the MIMIC-IV database available through the PhysioNet platform:
https://physionet.org/content/mimiciv/3.1/

No patient data is available via this repository. Prior to querying the MIMIC-IV database to derive the study cohort, additional concept tables were constructed via the code and instructions available from the MIT-LCP github repository:
https://github.com/MIT-LCP/mimic-code/tree/main/mimic-iv

## Suggested Citation

## Keywords
Machine learning, binary classification, clinical prediction, class imbalance, performance measures, electronic health records, simulation study, Monte Carlo simulation, sepsis, precision-recall AUC, hyperparameter tuning.

## Disclaimer
This code is provided for research purposes only and is not intended for clinical use. Access to MIMIC-IV requires PhysioNet credentialing and a signed data use agreement; users are responsible for complying with its terms, including not sharing patient-level data or attempting to re-identify individuals.

