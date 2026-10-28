library(dplyr)
library(xgboost)
library(lightgbm)
library(gt)
library(webshot2)


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


test <- readRDS("data/test.rds")
x_test <- dplyr::select(test, -death_within_30_days)
y_test <- test$death_within_30_days
test_performance_table <- readRDS("results/test_performance_table.rds")



# Identify best model per measure (min for Brier, max for all others)
best_accuracy   <- which.max(test_performance_table$Accuracy)
best_bal_acc    <- which.max(test_performance_table$Balanced_Accuracy)
best_sensitivity <- which.max(test_performance_table$Sensitivity)
best_specificity <- which.max(test_performance_table$Specificity)
best_ppv        <- which.max(test_performance_table$PPV)
best_npv        <- which.max(test_performance_table$NPV)
best_f1         <- which.max(test_performance_table$F1)
best_mcc        <- which.max(test_performance_table$MCC)
best_auc        <- which.max(test_performance_table$AUC)
best_brier      <- which.min(test_performance_table$Brier_Score)

test_performance_table_bold <- test_performance_table %>%
  gt() %>%
  fmt_number(
    columns = c(Accuracy, Balanced_Accuracy, Sensitivity, Specificity,
                PPV, NPV, F1, MCC, AUC, Brier_Score),
    decimals = 4
  ) %>%
  # Bold best per measure
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = Accuracy, rows = best_accuracy)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = Balanced_Accuracy, rows = best_bal_acc)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = Sensitivity, rows = best_sensitivity)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = Specificity, rows = best_specificity)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = PPV, rows = best_ppv)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = NPV, rows = best_npv)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = F1, rows = best_f1)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = MCC, rows = best_mcc)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = AUC, rows = best_auc)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = Brier_Score, rows = best_brier)) %>%
  # Clean column labels
  cols_label(
    Model             = "Algorithm",
    Balanced_Accuracy = "Balanced Accuracy",
    Brier_Score       = "Brier Score",
    F1                = "F1 Score"
  ) %>%
  cols_align(align = "right",   columns = Model) %>%
  cols_align(align = "center", columns = -Model) %>%
  tab_header(title = "Model Performance on Test Set") %>%
  tab_options(table.background.color = "white") %>%
  cols_width(
    Balanced_Accuracy ~ px(80),
    Brier_Score       ~ px(80),
    F1                ~ px(80)
  ) %>%
  gtsave("plots/test_performance_table_bold.png")

