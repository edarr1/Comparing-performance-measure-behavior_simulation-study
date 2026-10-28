#-------------------------------------------------------------------------------
#  Model Fitting
#-------------------------------------------------------------------------------

library(caret)
library(MASS)           # LDA
library(e1071)          # SVM
library(rpart)          # Decision tree
library(randomForest)   # Random Forest
library(xgboost)        # XGBoost
library(lightgbm)       # LightGBM
library(naivebayes)     # Naive Bayes
library(kknn)           # KNN
library(pROC)           # AUC
library(tidyverse)
library(ggplot2)
library(yardstick)
library(patchwork)
library(PRROC)



set.seed(123)

setwd(r"(C:\Users\emmad\Documents\Thesis)")
train <- readRDS("data/train.rds")
test <- readRDS("data/test.rds")

              ## Test/Train Data
x_train <- dplyr::select(train, -death_within_30_days)
y_train <- train$death_within_30_days

x_test <- dplyr::select(test, -death_within_30_days)
y_test <- test$death_within_30_days


# Calculate class weights from natural prevalence
n_total <- nrow(train)
n_pos   <- sum(train$death_within_30_days == 1)
n_neg   <- sum(train$death_within_30_days == 0)
w_pos   <- n_total / (2 * n_pos)  # inverse prevalence weighting
w_neg   <- n_total / (2 * n_neg)
scale_pos_weight <- n_neg / n_pos  # for XGBoost/LightGBM
# Create sample weights vector for models that use observation weights
sample_weights <- ifelse(train$death_within_30_days == 1, w_pos, w_neg)




              ## Model Fitting
## Logistic Regression
log_model <- glm(
  death_within_30_days ~ ., data = train,
  family = binomial, weights = sample_weights
  )
saveRDS(log_model,  "models/log_model.rds")

## SVM (linear kernel)
svm_model <- svm(
  x = x_train, y = as.factor(y_train), cost = 0.1,
  kernel = "linear", probability = TRUE,
  class.weights = c("0" = w_neg, "1" = w_pos)
)
saveRDS(svm_model, "models/svm_model.rds")

## Decision Tree
tree_model <- rpart(
  death_within_30_days ~ ., data = train,
  method = "class",
  weights = sample_weights
)
saveRDS(tree_model, "models/tree_model.rds")

## Random Forest
rf_model <- randomForest(
  death_within_30_days ~ ., data = train,
  ntree = 500,
  importance = T,
  classwt = c("0" = w_neg, "1" = w_pos)
)
saveRDS(rf_model, "models/rf_model.rds")

## XGBoost
# Convert to numeric matrix
x_train_mat <- as.matrix(x_train)
x_test_mat  <- as.matrix(x_test)
y_train_num <- as.numeric(y_train) - 1  # XGBoost expects 0/1

xgb_model <- xgboost(
  data = x_train_mat, label = y_train_num,
  nrounds = 100, objective = "binary:logistic",
  scale_pos_weight = scale_pos_weight, 
  verbose = 0
)
xgb.save(xgb_model, "models/xgb_model.xgbm")

## LightGBM
dtrain_lgb <- lgb.Dataset(
  data = x_train_mat,
  label = y_train_num
)

lgb_model <- lgb.train(
  params = list(
    objective = "binary", metric = "binary_logloss", 
    scale_pos_weight = scale_pos_weight, verbose = -1
  ),
  data = dtrain_lgb,
  nrounds = 100  # consistent with XGBoost nrounds
)
lgb.save(lgb_model, "models/lgb_model.txt")

## Naive Bayes
nb_model <- naive_bayes(
  x = x_train, y = as.factor(y_train),
)
saveRDS(nb_model, "models/nb_model.rds")

## KNN
knn_model <- train.kknn(
  death_within_30_days ~ ., data = train,
  kmax = 15, kernel = c("rectangular", "triangular", "gaussian"), distance = 2
)
saveRDS(knn_model, "models/knn_model.rds")


              ## Predictions on Test Set

# Logistic
pred_log_prob <- predict(log_model, newdata = x_test, type = "response")
pred_log <- ifelse(pred_log_prob > 0.5, 1, 0)

# SVM
pred_svm_prob <- attr(predict(svm_model, x_test, probability = TRUE), "probabilities")[,2]
pred_svm <- ifelse(pred_svm_prob > 0.5, 1, 0)

# Decision Tree
pred_tree_prob <- predict(tree_model, x_test, type = "prob")[,2]
pred_tree <- ifelse(pred_tree_prob > 0.5, 1, 0)

# Random Forest
pred_rf_prob <- predict(rf_model, x_test, type = "prob")[,2]
pred_rf <- ifelse(pred_rf_prob > 0.5, 1, 0)

# XGBoost
pred_xgb_prob <- predict(xgb_model, x_test_mat)
pred_xgb <- ifelse(pred_xgb_prob > 0.5, 1, 0)

# LightGBM
pred_lgb_prob <- predict(lgb_model, x_test_mat)
pred_lgb <- ifelse(pred_lgb_prob > 0.5, 1, 0)

# Naive Bayes
pred_nb_prob <- predict(nb_model, x_test, type = "prob")[,2]
pred_nb <- ifelse(pred_nb_prob > 0.5, 1, 0)

# KNN
pred_knn_prob <- predict(knn_model, x_test, type = "prob")[,2]
pred_knn <- ifelse(pred_knn_prob > 0.5, 1, 0)


## Check XGBoost and LightGBM probability distributions
#summary(predict(xgb_model, x_test_mat))
#summary(predict(lgb_model, x_test_mat))

                  ## Evaluate Models: Accuracy, Sensitivity, Specificity
conf_mat_log <- confusionMatrix(factor(pred_log), factor(y_test),positive = "1")
conf_mat_svm <- confusionMatrix(factor(pred_svm), factor(y_test),positive = "1")
conf_mat_tree <- confusionMatrix(factor(pred_tree), factor(y_test),positive = "1")
conf_mat_rf <- confusionMatrix(factor(pred_rf), factor(y_test),positive = "1")
conf_mat_xgb <- confusionMatrix(factor(pred_xgb), factor(y_test),positive = "1")
conf_mat_lgb <- confusionMatrix(factor(pred_lgb), factor(y_test),positive = "1")
conf_mat_nb <- confusionMatrix(factor(pred_nb), factor(y_test),positive = "1")
conf_mat_knn <- confusionMatrix(factor(pred_knn), factor(y_test),positive = "1")

conf_mat_log
conf_mat_svm
conf_mat_tree
conf_mat_rf
conf_mat_xgb
conf_mat_lgb
conf_mat_nb
conf_mat_knn

accuracy_results_full <- data.frame(
  Model = c("Logistic","SVM","Decision Tree","Random Forest","XGBoost","LightGBM",
            "Naive Bayes","KNN"),
  Accuracy = c(conf_mat_log$overall[1],
               conf_mat_svm$overall[1],
               conf_mat_tree$overall[1],
               conf_mat_rf$overall[1],
               conf_mat_xgb$overall[1],
               conf_mat_lgb$overall[1],
               conf_mat_nb$overall[1],
               conf_mat_knn$overall[1]),
  LCI = c(conf_mat_log$overall[3],
          conf_mat_svm$overall[3],
          conf_mat_tree$overall[3],
          conf_mat_rf$overall[3],
          conf_mat_xgb$overall[3],
          conf_mat_lgb$overall[3],
          conf_mat_nb$overall[3],
          conf_mat_knn$overall[3]),
  UCI = c(conf_mat_log$overall[4],
          conf_mat_svm$overall[4],
          conf_mat_tree$overall[4],
          conf_mat_rf$overall[4],
          conf_mat_xgb$overall[4],
          conf_mat_lgb$overall[4],
          conf_mat_nb$overall[4],
          conf_mat_knn$overall[4]),
  p_value = c(conf_mat_log$overall[6],
              conf_mat_svm$overall[6],
              conf_mat_tree$overall[6],
              conf_mat_rf$overall[6],
              conf_mat_xgb$overall[6],
              conf_mat_lgb$overall[6],
              conf_mat_nb$overall[6],
              conf_mat_knn$overall[6])
)
saveRDS(accuracy_results_full, "results/test_accuracy_table.rds")
write.csv(accuracy_results_full, "results/test_accuracy_table.csv", row.names = FALSE)

accuracy_results <- data.frame(
  Model = c("Logistic","SVM","Decision Tree","Random Forest","XGBoost","LightGBM",
            "Naive Bayes","KNN"),
  Accuracy = c(conf_mat_log$overall[1],
               conf_mat_svm$overall[1],
               conf_mat_tree$overall[1],
               conf_mat_rf$overall[1],
               conf_mat_xgb$overall[1],
               conf_mat_lgb$overall[1],
               conf_mat_nb$overall[1],
               conf_mat_knn$overall[1])
)

sensitivity_results <- data.frame(
  Model = c("Logistic","SVM","Decision Tree","Random Forest","XGBoost","LightGBM",
            "Naive Bayes","KNN"),
  Sensitivity = c(conf_mat_log$byClass[1],
               conf_mat_svm$byClass[1],
               conf_mat_tree$byClass[1],
               conf_mat_rf$byClass[1],
               conf_mat_xgb$byClass[1],
               conf_mat_lgb$byClass[1],
               conf_mat_nb$byClass[1],
               conf_mat_knn$byClass[1])
)

specificity_results <- data.frame(
  Model = c("Logistic","SVM","Decision Tree","Random Forest","XGBoost","LightGBM",
            "Naive Bayes","KNN"),
  Specificity = c(conf_mat_log$byClass[2],
                  conf_mat_svm$byClass[2],
                  conf_mat_tree$byClass[2],
                  conf_mat_rf$byClass[2],
                  conf_mat_xgb$byClass[2],
                  conf_mat_lgb$byClass[2],
                  conf_mat_nb$byClass[2],
                  conf_mat_knn$byClass[2])
)

ppv_results <- data.frame(
  Model = c("Logistic","SVM","Decision Tree","Random Forest","XGBoost","LightGBM",
            "Naive Bayes","KNN"),
  PPV = c(conf_mat_log$byClass[3],
          conf_mat_svm$byClass[3],
          conf_mat_tree$byClass[3],
          conf_mat_rf$byClass[3],
          conf_mat_xgb$byClass[3],
          conf_mat_lgb$byClass[3],
          conf_mat_nb$byClass[3],
          conf_mat_knn$byClass[3])
)

npv_results <- data.frame(
  Model = c("Logistic","SVM","Decision Tree","Random Forest","XGBoost","LightGBM",
            "Naive Bayes","KNN"),
  NPV = c(conf_mat_log$byClass[4],
          conf_mat_svm$byClass[4],
          conf_mat_tree$byClass[4],
          conf_mat_rf$byClass[4],
          conf_mat_xgb$byClass[4],
          conf_mat_lgb$byClass[4],
          conf_mat_nb$byClass[4],
          conf_mat_knn$byClass[4])
)

f1_results <- data.frame(
  Model = c("Logistic","SVM","Decision Tree","Random Forest","XGBoost","LightGBM",
            "Naive Bayes","KNN"),
  F1 = c(conf_mat_log$byClass[7],
         conf_mat_svm$byClass[7],
         conf_mat_tree$byClass[7],
         conf_mat_rf$byClass[7],
         conf_mat_xgb$byClass[7],
         conf_mat_lgb$byClass[7],
         conf_mat_nb$byClass[7],
         conf_mat_knn$byClass[7])
)

balanced_accuracy_results <- data.frame(
  Model = c("Logistic","SVM","Decision Tree","Random Forest","XGBoost","LightGBM",
            "Naive Bayes","KNN"),
  Balanced_Accuracy = c(conf_mat_log$byClass[11],
                        conf_mat_svm$byClass[11],
                        conf_mat_tree$byClass[11],
                        conf_mat_rf$byClass[11],
                        conf_mat_xgb$byClass[11],
                        conf_mat_lgb$byClass[11],
                        conf_mat_nb$byClass[11],
                        conf_mat_knn$byClass[11])
)




print(accuracy_results)
print(sensitivity_results)
print(specificity_results)
print(ppv_results)
print(npv_results)
print(f1_results)
print(balanced_accuracy_results)



#visualization
log_cm_plot <- data.frame(obs = y_test, pred = as.factor(pred_log))
log_cm <- conf_mat(log_cm_plot, truth = obs, estimate = pred)

svm_cm_plot <- data.frame(obs = y_test, pred = as.factor(pred_svm))
svm_cm <- conf_mat(svm_cm_plot, truth = obs, estimate = pred)

tree_cm_plot <- data.frame(obs = y_test, pred = as.factor(pred_tree))
tree_cm <- conf_mat(tree_cm_plot, truth = obs, estimate = pred)
  
rf_cm_plot <- data.frame(obs = y_test, pred = as.factor(pred_rf))
rf_cm <- conf_mat(rf_cm_plot, truth = obs, estimate = pred)

xgb_cm_plot <- data.frame(obs = y_test, pred = as.factor(pred_xgb))
xgb_cm <- conf_mat(xgb_cm_plot, truth = obs, estimate = pred)

lgb_cm_plot <- data.frame(obs = y_test, pred = as.factor(pred_lgb))
lgb_cm <- conf_mat(lgb_cm_plot, truth = obs, estimate = pred)

nb_cm_plot <- data.frame(obs = y_test, pred = as.factor(pred_nb))
nb_cm <- conf_mat(nb_cm_plot, truth = obs, estimate = pred)

knn_cm_plot <- data.frame(obs = y_test, pred = as.factor(pred_knn))
knn_cm <- conf_mat(knn_cm_plot, truth = obs, estimate = pred)



cm_objects <- list(
  Logistic      = log_cm,
  SVM           = svm_cm,
  Decision_Tree = tree_cm,
  Random_Forest = rf_cm,
  XGBoost       = xgb_cm,
  LightGBM      = lgb_cm,
  Naive_Bayes   = nb_cm,
  KNN           = knn_cm
)

for(model_name in names(cm_objects)){
  p <- autoplot(cm_objects[[model_name]], type = "heatmap") +
    scale_fill_gradient2(high = "tomato4") +
    ggtitle(gsub("_", " ", model_name)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(
    filename = paste0("plots/confusion_matrix/", tolower(model_name), "_cm.png"),
    plot     = p,
    width    = 6,
    height   = 5,
    dpi      = 300
  )
}

# Arrange 8 plots in 2 rows x 4 columns
cm_plots <- lapply(names(cm_objects), function(model_name){
  autoplot(cm_objects[[model_name]], type = "heatmap") +
    scale_fill_gradient2(high = "tomato4") +
    ggtitle(gsub("_", " ", model_name)) +
    theme_bw() +
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold", size = 10),
      axis.text    = element_text(size = 8),
      axis.title   = element_text(size = 8),
      legend.position = "none"  # remove individual legends for clean vignette
    )
})


cm_vignette <- wrap_plots(cm_plots, nrow = 2, ncol = 4) +
  plot_annotation(
    title    = "Confusion Matrices — Test Set Performance",
    subtitle = "All eight algorithms at threshold = 0.5",
    theme    = theme(
      plot.title    = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11)
    )
  )

ggsave("plots/confusion_matrix/cm_vignette.png", cm_vignette,
       width = 20, height = 10, dpi = 300)


## Evaluate Models: AUC
auc_log  <- roc(y_test, pred_log_prob)$auc
auc_svm  <- roc(y_test, pred_svm_prob)$auc
auc_tree <- roc(y_test, pred_tree_prob)$auc
auc_rf   <- roc(y_test, pred_rf_prob)$auc
auc_xgb  <- roc(y_test, pred_xgb_prob)$auc
auc_lgb  <- roc(y_test, pred_lgb_prob)$auc
auc_nb   <- roc(y_test, pred_nb_prob)$auc
auc_knn  <- roc(y_test, pred_knn_prob)$auc

auc_results <- data.frame(
  Model = c("Logistic","SVM","Decision Tree","Random Forest","XGBoost","LightGBM",
            "Naive Bayes","KNN"),
  AUC = c(auc_log, auc_svm, auc_tree, auc_rf, auc_xgb, auc_lgb, auc_nb, auc_knn)
)
print(auc_results)


                  ## Brier Score
y_test_num <- as.numeric(as.character(y_test))

brier_score_log  <- mean((pred_log_prob  - y_test_num)^2)
brier_score_svm  <- mean((pred_svm_prob  - y_test_num)^2)
brier_score_tree <- mean((pred_tree_prob - y_test_num)^2)
brier_score_rf   <- mean((pred_rf_prob   - y_test_num)^2)
brier_score_xgb  <- mean((pred_xgb_prob  - y_test_num)^2)
brier_score_lgb  <- mean((pred_lgb_prob  - y_test_num)^2)
brier_score_nb   <- mean((pred_nb_prob   - y_test_num)^2)
brier_score_knn  <- mean((pred_knn_prob  - y_test_num)^2)

brier_score_results <- data.frame(
  Model = c("Logistic","SVM","Decision Tree","Random Forest","XGBoost","LightGBM",
            "Naive Bayes","KNN"),
  Brier_Score = c(brier_score_log, brier_score_svm, brier_score_tree, brier_score_rf,
                 brier_score_xgb, brier_score_lgb, brier_score_nb, brier_score_knn)
)
print(brier_score_results)



                  ## MCC

compute_mcc <- function(y_true, y_pred){
  tp <- sum(y_pred == 1 & y_true == 1)
  tn <- sum(y_pred == 0 & y_true == 0)
  fp <- sum(y_pred == 1 & y_true == 0)
  fn <- sum(y_pred == 0 & y_true == 1)
  
  numerator   <- as.numeric(tp) * as.numeric(tn) - as.numeric(fp) * as.numeric(fn)
  denominator <- sqrt((as.numeric(tp) + as.numeric(fp)) *
                        (as.numeric(tp) + as.numeric(fn)) *
                        (as.numeric(tn) + as.numeric(fp)) *
                        (as.numeric(tn) + as.numeric(fn)))
  
  if(denominator == 0) return(NA)
  return(numerator / denominator)
}

mcc_results <- data.frame(
  Model = c("Logistic","SVM","Decision Tree","Random Forest",
            "XGBoost","LightGBM","Naive Bayes","KNN"),
  MCC = c(
    compute_mcc(y_test, pred_log),
    compute_mcc(y_test, pred_svm),
    compute_mcc(y_test, pred_tree),
    compute_mcc(y_test, pred_rf),
    compute_mcc(y_test, pred_xgb),
    compute_mcc(y_test, pred_lgb),
    compute_mcc(y_test, pred_nb),
    compute_mcc(y_test, pred_knn)
  )
)

print(mcc_results)



# combined results table
test_performance_table <- list(
  accuracy_results,
  balanced_accuracy_results,
  sensitivity_results,
  specificity_results,
  ppv_results,
  npv_results,
  f1_results,
  mcc_results,
  auc_results,
  brier_score_results
) %>%
  reduce(left_join, by = "Model")

print(test_performance_table)
saveRDS(test_performance_table, "results/test_performance_table.rds")
write.csv(test_performance_table, "results/test_performance_table.csv", row.names = FALSE)



## ROC plot
roc_list <- list(
  "Logistic Regression" = roc(y_test, pred_log_prob,  quiet = TRUE),
  "SVM"                 = roc(y_test, pred_svm_prob,  quiet = TRUE),
  "Decision Tree"       = roc(y_test, pred_tree_prob, quiet = TRUE),
  "Random Forest"       = roc(y_test, pred_rf_prob,   quiet = TRUE),
  "XGBoost"             = roc(y_test, pred_xgb_prob,  quiet = TRUE),
  "LightGBM"            = roc(y_test, pred_lgb_prob,  quiet = TRUE),
  "Naive Bayes"         = roc(y_test, pred_nb_prob,   quiet = TRUE),
  "KNN"                 = roc(y_test, pred_knn_prob,  quiet = TRUE)
)

roc_df <- do.call(rbind, lapply(names(roc_list), function(m){
  roc_obj <- roc_list[[m]]
  data.frame(
    model       = m,
    fpr         = 1 - roc_obj$specificities,
    tpr         = roc_obj$sensitivities,
    auc         = as.numeric(roc_obj$auc)
  )
}))

# AUC labels for legend
auc_labels <- sapply(names(roc_list), function(m){
  paste0(m, " (AUC = ", round(as.numeric(roc_list[[m]]$auc), 3), ")")
})
roc_df$model_auc <- auc_labels[roc_df$model]
# Set consistent order
model_order <- c(
  "Logistic Regression",
  "SVM",
  "Decision Tree",
  "Random Forest",
  "XGBoost",
  "LightGBM",
  "Naive Bayes",
  "KNN"
)
# Reorder auc_labels to match
auc_labels_ordered <- auc_labels[model_order]
roc_df$model_auc <- factor(roc_df$model_auc, 
                           levels = auc_labels_ordered)


p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr, color = model_auc)) +
  geom_line(linewidth = 0.8) +
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed", color = "grey50", linewidth = 0.7) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  labs(
    title    = "ROC Curves — Test Set Performance",
    subtitle = "All algorithms at default threshold",
    x        = "False Positive Rate (1 - Specificity)",
    y        = "True Positive Rate (Sensitivity)",
    color    = "Algorithm"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.75, 0.25),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold")
  )

ggsave("plots/roc_curves.png", p_roc, width = 10, height = 8, dpi = 300)





