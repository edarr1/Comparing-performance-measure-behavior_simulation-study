# Preprocessing and Simulation Study

## Order
1) load_mimic_database
2) preprocessing_condensed
3) model_fitting
4) dgm_parameters
5) simulation_loop
6) reference_data
7) performance_summary
8) ranking_consistency

## 1) load_mimic_database.R
Connects to user's local copy of the MIMIC-IV database. Database specifications will need to be altered to be user specific. After connecting to the local MIMIC-IV database in R, saves the final sepsis cohort table (sepsis_final) as both .csv and .rds files.

## 2) preprocessing_condensed.R
Contains code for cleaning the final sepsis cohort data and preprocessing the data for both training machine learning models and extracting simulation study parameters. An additional script, 'preprocessing_full.R', is also included in this respository, and includes all steps of the 'preprocessing_condensed.R' script as well as additional, exploratory code that was not necessary for processing the cohort data.

### condensed preprocessing:
- update data types of several variables
- remove features with > 10% missing values ('crp_max','mean_height')
- impute 'vent_status' missing values as '0' (not on ventilator)
- remove rows with duplicate 'stay_id' and 'subject_id'
- remove identifier variables ('stay_id', 'subject_id')
- remove variables with potential data leakage ('los_icu', 'loc_hospital')
- split data into 80:20 (training:testing)
- impute missing values, dummy code integers, and remove variables with near zero variance (using R recipes package)
- remove several variables to address collinearity
- Yeo Johnson transformation and scaling of non-binary variables (using R recipes package)
- save final training and testing sets as .rds files

### additional preprocessing:
The 'preprocessing_full.R' script contains the following additional steps:
- identifying variables with high missing values
- identifying duplicate patient rows
- identifying highly correlated variables (r > 0.8)
- VIF analysis (no high VIF)
- feature selection (keeps all variables)

## 3) model_fitting.R
After loading the preprocessed data, fits eight classification machine learning models to the training set to predict an outcome of 30-day mortality. Models are then evaluated on the testing data, and performance measures calculated for each model. Confusion matrices and ROC curves created for each model. Final models saved as .rds files.

## 4) dgm_parameters.R
Using the same training data as the model training, this script extracts the marginal parameters, estimates the correlation matrix, and corrects for positive definitiness, to use as parameters for the simulation study data generating mechanism.

## 5) simulation_loop.R
After loading saved models and dgm parameters, creates a logistic regression outcome mechanism based on the same training data, and creates a Gaussian copula based on the dgm parameters to generate patient data. Then loops through 1000 simulation rounds of 5000 observations, predicting 30-day mortality for each model and saving the ten performance measures for each model per each simulation round. Also saves RNG states for each simulation round.

## 6) reference_data.R 
Uses the same Guassian copula from the simulation loop to generate a large dataset of 100,000 observations; used to calculate estimand values for each performance measures for reach model.

## 7) performance_summary.R 
Using simulation results and estimand values from the reference dataset, calculates the bias, Monte Carlo (empirical) variance, Monte Carlo (empirical) standard error, and root mean squared error for each performance measure/model combination.

## 8) ranking_consistency.R
Calculates mean Spearman correlation between model rankings for each performance measure.

## visualization.R 
Density and boxplots for each performance measure.\
Bias per model per performance measure.\
Convergence plots for each performance measure.\
Bias, EmpSE, and RMSE per performance measure.\
Bias, EmpSE, and RMSE per model algorithm.

## test_performance_table.R 
Summarizes model performance on the empirical test set.