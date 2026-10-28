
## This script generates a large reference data set, based off the previously
## saved Gaussian-Copula parameters, in order to determine true estimand values

library(pROC)
library(copula)
library(caret)
library(e1071)          # SVM
library(rpart)          # Decision tree
library(randomForest)   # Random Forest
library(xgboost)        # XGBoost
library(lightgbm)       # LightGBM
library(naivebayes)     # Naive Bayes
library(kknn)           # KNN


set.seed(456) #intentionally different from simulation loop seed
setwd(r"(C:\Users\emmad\Documents\Thesis)")

dgm_params <- readRDS("data/dgm_params.rds")
dgm_outcome_model <- readRDS("models/dgm_outcome_model.rds")
dgm_coefs <- coef(dgm_outcome_model)
compute_measures <- readRDS("results/formulas/compute_measures.rds")

log_model <- readRDS("models/log_model.rds")
svm_model <- readRDS("models/svm_model.rds")
tree_model <- readRDS("models/tree_model.rds")
rf_model <- readRDS("models/rf_model.rds")
nb_model  <- readRDS("models/nb_model.rds")
knn_model <- readRDS("models/knn_model.rds")
xgb_model <- xgb.load("models/xgb_model.xgbm")
lgb_model <- lgb.load("models/lgb_model.txt")

# Define Gaussian copula from saved parameters 
gauss_cop <- normalCopula(
  param = P2p(dgm_params$cor_matrix_pd),
  dim = dgm_params$n_vars,
  dispstr = "un"
)



# Reference dataset
n_ref <- 100000

# Step 1: Simulate covariates from Gaussian copula
u_ref  <- rCopula(n_ref, gauss_cop)
x_ref  <- matrix(NA, nrow = n_ref, ncol = dgm_params$n_vars)

# Continuous variables
for(j in seq_along(dgm_params$cont_idx)){
  col_j         <- dgm_params$cont_idx[j]
  x_ref[, col_j] <- qnorm(
    u_ref[, col_j],
    mean = dgm_params$cont_mean[j],
    sd   = dgm_params$cont_sd[j]
  )
}

# Clamp continuous variables to training range
for(j in seq_along(dgm_params$cont_idx)){
  col_j          <- dgm_params$cont_idx[j]
  x_ref[, col_j] <- pmin(
    pmax(x_ref[, col_j], dgm_params$cont_min[j]),
    dgm_params$cont_max[j]
  )
}

# Binary variables
for(j in seq_along(dgm_params$bin_idx)){
  col_j         <- dgm_params$bin_idx[j]
  p_j           <- dgm_params$bin_prop[j]
  x_ref[, col_j] <- as.numeric(u_ref[, col_j] > (1 - p_j))
}

x_ref_matrix <- x_ref
x_ref <- as.data.frame(x_ref)
colnames(x_ref) <- dgm_params$var_names

# Step 2: Generate outcomes from known logistic mechanism
x_ref_mat <- as.matrix(cbind(1, x_ref))
log_odds_ref <- x_ref_mat %*% dgm_coefs
p_ref <- pmin(pmax(1 / (1 + exp(-log_odds_ref)), 1e-10), 1 - 1e-10)
y_ref <- rbinom(n_ref, 1, p_ref)

# Check prevalence
cat("Reference dataset prevalence:", round(mean(y_ref), 3), "\n")

# Step 4: Generate probabilities from each frozen model
ref_probs <- list(
  log  = predict(log_model, newdata = x_ref, type = "response"),
  svm  = attr(predict(svm_model, x_ref, probability = TRUE), "probabilities")[, 2],
  tree = predict(tree_model, x_ref, type = "prob")[, 2],
  rf   = predict(rf_model, x_ref, type = "prob")[, 2],
  xgb  = predict(xgb_model, x_ref_matrix),
  lgb  = predict(lgb_model, x_ref_matrix),
  nb   = predict(nb_model, x_ref, type = "prob")[, 2],
  knn  = predict(knn_model, x_ref, type = "prob")[, 2]
)

# Step 5: Compute true estimands for each model
models   <- c("log", "svm", "tree", "rf", "xgb", "lgb", "nb", "knn")
measures <- c("accuracy", "balanced_accuracy", "sensitivity", "specificity",
              "ppv", "npv", "f1", "mcc", "auc", "brier")
true_estimands <- sapply(models, function(m){
  compute_measures(y_ref, ref_probs[[m]])
})

# Format as data frame
true_estimands <- as.data.frame(t(true_estimands))
rownames(true_estimands) <- models
colnames(true_estimands) <- measures

# Save
saveRDS(true_estimands, "results/true_estimands.rds")

# Preview
print(round(true_estimands, 4))
