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



set.seed(123)
setwd(r"(C:\Users\emmad\Documents\Thesis)")

# Load models
log_model <- readRDS("models/log_model.rds")
svm_model <- readRDS("models/svm_model.rds")
tree_model <- readRDS("models/tree_model.rds")
rf_model <- readRDS("models/rf_model.rds")
nb_model  <- readRDS("models/nb_model.rds")
knn_model <- readRDS("models/knn_model.rds")
xgb_model <- xgb.load("models/xgb_model.xgbm")
lgb_model <- lgb.load("models/lgb_model.txt")

# Load train_sim
train_sim <- readRDS("data/train_sim.rds")
x_train <- dplyr::select(train_sim, -death_within_30_days)
y_train <- train_sim$death_within_30_days

# Helper functions
# Helper function: compute all measures 
compute_measures <- function(y_true, y_prob){
  y_pred <- ifelse(y_prob > 0.5, 1, 0)
  y_true <- as.numeric(as.character(y_true))
  
  # Confusion matrix components
  tp <- sum(y_pred == 1 & y_true == 1)
  tn <- sum(y_pred == 0 & y_true == 0)
  fp <- sum(y_pred == 1 & y_true == 0)
  fn <- sum(y_pred == 0 & y_true == 1)
  
  # Measures
  accuracy          <- (tp + tn) / (tp + tn + fp + fn)
  balanced_accuracy <- ((tp / (tp + fn)) + (tn / (tn + fp))) / 2
  sensitivity       <- tp / (tp + fn)
  specificity       <- tn / (tn + fp)
  ppv               <- ifelse((tp + fp) == 0, NA, tp / (tp + fp))
  npv               <- ifelse((tn + fn) == 0, NA, tn / (tn + fn))
  f1                <- (2 * tp) / (2 * tp + fp + fn)
  mcc_denom         <- sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) *
                              as.numeric(tn + fp) * as.numeric(tn + fn))
  mcc               <- ifelse(mcc_denom == 0, 0, 
                              (as.numeric(tp) * as.numeric(tn) - 
                                 as.numeric(fp) * as.numeric(fn)) / mcc_denom)
  auc               <- as.numeric(roc(y_true, y_prob, quiet = TRUE)$auc)
  brier             <- mean((y_prob - y_true)^2)
  
  return(c(accuracy, balanced_accuracy, sensitivity, specificity,
           ppv, npv, f1, mcc, auc, brier))
}
saveRDS(compute_measures, "results/formulas/compute_measures.rds")

# Load saved DGM parameters 
dgm_params <- readRDS("data/dgm_params.rds")


# Fit logistic model for outcome mechanism
dgm_outcome_model <- glm(
  death_within_30_days ~ ., data = train_sim, family = binomial
)
dgm_coefs <- coef(dgm_outcome_model)
saveRDS(dgm_outcome_model, "models/dgm_outcome_model.rds")

# Define Gaussian copula from saved parameters 
gauss_cop <- normalCopula(
  param = P2p(dgm_params$cor_matrix_pd),
  dim = dgm_params$n_vars,
  dispstr = "un"
)

# Simulation settings 
n_sim <- 1000
n_obs <- 5000

# Results storage 
models <- c("log", "svm", "tree", "rf", "xgb", "lgb", "nb", "knn")
measures <- c("accuracy", "balanced_accuracy", "sensitivity", "specificity",
              "ppv", "npv", "f1", "mcc", "auc", "brier")

# 3D array: n_sim x measures x models
results <- array(
  NA,
  dim = c(n_sim, length(measures), length(models)),
  dimnames = list(
    paste0("sim", 1:n_sim),
    measures,
    models
  )
)




# Storage for RNG states
rng_states <- vector("list", n_sim + 1)
# Store initial state before loop
rng_states[[1]] <- .Random.seed


# Simulation loop 

for(i in 1:n_sim){
  
  # Store RNG state at start of each repetition
  rng_states[[i]] <- .Random.seed
  
  # Step 1: Simulate covariates from Gaussian copula
  u_sim <- rCopula(n_obs, gauss_cop)
  x_sim <- matrix(NA, nrow = n_obs, ncol = dgm_params$n_vars)
  
  # Continuous variables — inverse normal transform
  for(j in seq_along(dgm_params$cont_idx)){
    col_j          <- dgm_params$cont_idx[j]
    x_sim[, col_j] <- qnorm(
      u_sim[, col_j],
      mean = dgm_params$cont_mean[j],
      sd   = dgm_params$cont_sd[j]
    )
  }
  
  # Clamp continuous variables to training range
  for(j in seq_along(dgm_params$cont_idx)){
    col_j         <- dgm_params$cont_idx[j]
    x_sim[, col_j] <- pmin(
      pmax(x_sim[, col_j], dgm_params$cont_min[j]),
      dgm_params$cont_max[j]
    )
  }
  
  # Binary variables — inverse Bernoulli transform
  for(j in seq_along(dgm_params$bin_idx)){
    col_j          <- dgm_params$bin_idx[j]
    p_j            <- dgm_params$bin_prop[j]  # position in binary list, not full index
    x_sim[, col_j] <- as.numeric(u_sim[, col_j] > (1 - p_j))
  }
  
  x_sim <- as.data.frame(x_sim)
  colnames(x_sim) <- dgm_params$var_names
  
  # Step 2: Generate outcomes from known logistic mechanism
  x_sim_mat <- as.matrix(cbind(1, x_sim))
  log_odds  <- x_sim_mat %*% dgm_coefs
  p_sim <- pmin(pmax(1 / (1 + exp(-log_odds)), 1e-10), 1 - 1e-10)
  y_sim     <- rbinom(n_obs, 1, p_sim)
  
  # Step 3: Prepare matrices for XGBoost/LightGBM
  x_sim_matrix <- as.matrix(x_sim)
  
  # Step 4: Generate probabilities from each frozen model
  probs <- list(
    log = predict(log_model, newdata = x_sim, type = "response"),
    svm = attr(predict(svm_model, x_sim, probability = TRUE), "probabilities")[, 2],
    tree = predict(tree_model, x_sim, type = "prob")[, 2],
    rf  = predict(rf_model, x_sim, type = "prob")[, 2],
    xgb = predict(xgb_model, x_sim_matrix),
    lgb = predict(lgb_model, x_sim_matrix),
    nb  = predict(nb_model, x_sim, type = "prob")[, 2],
    knn = predict(knn_model, x_sim, type = "prob")[, 2]
  )
  
  # Step 5: Compute measures for each model
  for(m in models){
    results[i, , m] <- compute_measures(y_sim, probs[[m]])
  }
  
  # Progress tracking
  # Checkpoint every 100 simulations
  if(i %% 100 == 0){
    cat("Simulation", i, "of", n_sim, "complete\n")
    saveRDS(results,    "results/simulation_results_checkpoint.rds")
    saveRDS(rng_states, "results/rng_states_checkpoint.rds")
  }
}


# Save results 
saveRDS(results, "results/simulation_results.rds")

