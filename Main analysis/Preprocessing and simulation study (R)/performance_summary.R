library(tidyverse)
library(gt)
library(webshot2)
library(dplyr)

setwd(r"(C:\Users\emmad\Documents\Thesis)")

results <- readRDS("results/simulation_results.rds")
true_estimands <- readRDS("results/true_estimands.rds")

models  <- c("log", "svm", "tree", "rf", "xgb", "lgb", "nb", "knn")
measures <- c("accuracy", "balanced_accuracy", "sensitivity", "specificity",
              "ppv", "npv", "f1", "mcc", "auc", "brier")

# Compute Bias, MC Variance, RMSE, MCSE
compute_performance <- function(results, true_estimands, models, measures){
  
  perf_list <- list()
  
  for(m in models){
    for(k in measures){
      
      # Extract simulation values for this model/measure
      sim_vals <- results[, k, m]
      true_val <- true_estimands[m, k]
      
      # Remove NAs
      sim_vals <- sim_vals[!is.na(sim_vals)]
      n_sim    <- length(sim_vals)
      
      # Bias
      bias <- mean(sim_vals) - true_val
      
      # MC Variance
      mc_var <- var(sim_vals)
      
      # MC Standard deviation (EmpSE)
      emp_se <- sqrt(mc_var)
      
      # MSE
      mse <- mean((sim_vals - true_val)^2)
      
      # RMSE
      rmse <- sqrt(mse)
      
      # MCSE of bias (Monte Carlo Standard Error)
      mcse_bias <- emp_se / sqrt(n_sim)
      
      # MCSE of EmpSE
      mcse_empse <- mean(sqrt(mc_var)) / sqrt(2 * (1000 - 1))
      
      # MCSE of MSE
      squared_errors <- (sim_vals - true_val)^2
      mcse_mse <- sqrt(sum((squared_errors - mse)^2) / (n_sim * (n_sim - 1)))
      
      # MCSE of RMSE — derived via delta method
      mcse_rmse <- mcse_mse / (2 * sqrt(mse))
      
      perf_list[[paste(m, k, sep = "_")]] <- data.frame(
        model      = m,
        measure    = k,
        true_val   = true_val,
        mean_sim   = mean(sim_vals),
        bias       = bias,
        mc_var     = mc_var,
        emp_se     = emp_se,
        rmse       = rmse,
        mcse_bias  = mcse_bias, 
        mcse_empse = mcse_empse,
        mcse_rmse  = mcse_rmse,
        n_sim      = n_sim
      )
    }
  }
  
  do.call(rbind, perf_list)
}

saveRDS(compute_performance,"results/formulas/compute_performance.rds")

perf_table <- compute_performance(results, true_estimands, models, measures)
rownames(perf_table) <- NULL

saveRDS(perf_table, "results/performance_table.rds")
write.csv(perf_table, "results/performance_table.csv", row.names = FALSE)



# Summary by measure (acrossmodels)
summary_by_measure <- perf_table %>%
  group_by(measure) %>%
  summarise(
    mean_bias    = mean(abs(bias)),
    mean_mc_var  = mean(mc_var),
    mean_rmse    = mean(rmse),
    max_bias     = max(abs(bias)),
    min_bias     = min(abs(bias)),
    .groups = "drop"
  )

print(summary_by_measure)
saveRDS(summary_by_measure, "results/summary_by_measure.rds")
write.csv(summary_by_measure, "results/summary_by_measure.csv", row.names = FALSE)


# summary by measure with ci
ci_table_measure <- perf_table %>%
  group_by(measure) %>%
  summarise(
    # Bias
    mean_bias     = mean(bias),
    mcse_bias     = mean(mcse_bias),
    ci_lower      = mean(bias) - 1.96 * mean(mcse_bias),
    ci_upper      = mean(bias) + 1.96 * mean(mcse_bias),
    max_bias      = max(abs(bias)),
    min_bias      = min(abs(bias)),
    
    # EmpSE (sqrt of MC variance)
    mean_empse     = mean(sqrt(mc_var)),
    mcse_empse     = mean(sqrt(mc_var)) / sqrt(2 * (1000 - 1)),
    empse_ci_lower = mean(sqrt(mc_var)) - 1.96 * (mean(sqrt(mc_var)) / sqrt(2 * (1000 - 1))),
    empse_ci_upper = mean(sqrt(mc_var)) + 1.96 * (mean(sqrt(mc_var)) / sqrt(2 * (1000 - 1))),
    max_empse      = max(sqrt(mc_var)),
    min_empse      = min(sqrt(mc_var)),
    
    #RMSE
    mean_rmse     = mean(rmse),
    mcse_rmse     = mean(mcse_rmse),
    rmse_ci_lower = mean(rmse) - 1.96 * mean(mcse_rmse),
    rmse_ci_upper = mean(rmse) + 1.96 * mean(mcse_rmse),
    max_rmse      = max(rmse),
    min_rmse      = min(rmse),
    .groups       = "drop"
  )

print(ci_table_measure)
saveRDS(ci_table_measure, "results/ci_table_measure.rds")
write.csv(ci_table_measure, "results/ci_table_measure.csv", row.names = FALSE)


# summary by measure with ci and heatmap
ci_table_measure_heatmap <- 
  ci_table_measure %>%
  dplyr::select(measure, mean_bias, ci_lower, ci_upper, mean_empse,
         empse_ci_lower, empse_ci_upper, mean_rmse, 
         rmse_ci_lower, rmse_ci_upper) %>%  
  mutate(measure = case_when(
    measure == "accuracy"          ~ "Accuracy",
    measure == "auc"               ~ "AUC",
    measure == "balanced_accuracy" ~ "Balanced Accuracy",
    measure == "brier"             ~ "Brier Score",
    measure == "f1"                ~ "F1 Score",
    measure == "mcc"               ~ "MCC",
    measure == "npv"               ~ "NPV",
    measure == "ppv"               ~ "PPV",
    measure == "sensitivity"       ~ "Sensitivity",
    measure == "specificity"       ~ "Specificity"
  ),
  bias_ci    = paste0("(", round(ci_lower, 5), ", ", format(round(ci_upper, 5, ), scientific = FALSE), ")"),
  empse_ci   = paste0("(", round(empse_ci_lower, 5), ", ", round(empse_ci_upper, 5), ")"),
  rmse_ci    = paste0("(", round(rmse_ci_lower, 5), ", ", round(rmse_ci_upper, 5), ")")) %>%
  dplyr::select(measure, mean_bias, bias_ci, mean_empse, empse_ci, mean_rmse, rmse_ci) %>%
  gt() %>%
  fmt_number(decimals = 5) %>% 
  # Color bias column — higher absolute bias = darker red
  data_color(
    columns = mean_bias,
    fn = scales::col_numeric(
      palette = c("white", "tomato3"),
      domain  = NULL
    )
  ) %>% 
  data_color(
    columns = mean_empse,
    fn = scales::col_numeric(
      palette = c("white", "tomato3"),
      domain  = NULL
    ) 
  ) %>%
  # Color RMSE column
  data_color(
    columns = mean_rmse,
    fn = scales::col_numeric(
      palette = c("white", "tomato3"),
      domain  = NULL
    )
  )  %>%
  cols_label(
    measure    = "Measure",
    mean_bias  = "Estimate",
    bias_ci    = "95% CI",
    mean_empse = "Estimate",
    empse_ci   = "95% CI",
    mean_rmse  = "Estimate",
    rmse_ci    = "95% CI"
  ) %>%
  cols_align(align = "right",  columns = measure) %>%
  cols_align(align = "center", columns = -measure) %>%
  tab_spanner(label = "Bias",  columns = c(mean_bias, bias_ci)) %>%
  tab_spanner(label = "EmpSE", columns = c(mean_empse, empse_ci)) %>%
  tab_spanner(label = "RMSE",  columns = c(mean_rmse, rmse_ci)) %>%
  tab_header(title = "Performance Measure Estimation Error") %>%
  cols_width(measure ~ px(100)) %>%
  gtsave("plots/ci_measure_table.png")

# summary by measure with ci and bold

# Identify worst performers
worst_bias <- which.max(abs(ci_table_measure$mean_bias))
worst_empse <- which.max(ci_table_measure$mean_empse)
worst_rmse <- which.max(ci_table_measure$mean_rmse)

ci_table_measure_bold <- 
  ci_table_measure %>%
  dplyr::select(measure, mean_bias, ci_lower, ci_upper, mean_empse,
                empse_ci_lower, empse_ci_upper, mean_rmse, 
                rmse_ci_lower, rmse_ci_upper) %>%  
  mutate(
    measure = case_when(
      measure == "accuracy"          ~ "Accuracy",
      measure == "auc"               ~ "ROC-AUC",
      measure == "balanced_accuracy" ~ "Balanced Accuracy",
      measure == "brier"             ~ "Brier Score",
      measure == "f1"                ~ "F1 Score",
      measure == "mcc"               ~ "MCC",
      measure == "npv"               ~ "NPV",
      measure == "ppv"               ~ "PPV",
      measure == "sensitivity"       ~ "Sensitivity",
      measure == "specificity"       ~ "Specificity"
    ),
    bias_ci    = paste0("(", round(ci_lower, 5), ", ", format(round(ci_upper, 5),scientific = FALSE), ")"),
    empse_ci   = paste0("(", round(empse_ci_lower, 5), ", ", round(empse_ci_upper, 5), ")"),
    rmse_ci    = paste0("(", round(rmse_ci_lower, 5), ", ", round(rmse_ci_upper, 5), ")")
  ) %>%
  mutate(measure = factor(measure, levels = c(
    "Accuracy",
    "Balanced Accuracy",
    "Sensitivity",
    "Specificity",
    "PPV",
    "NPV",
    "F1 Score",
    "MCC",
    "ROC-AUC",
    "Brier Score"
  ))) %>%
  arrange(measure) %>%
  dplyr::select(measure, mean_bias, bias_ci, mean_empse, empse_ci, mean_rmse, rmse_ci) %>%
  gt() %>%
  fmt_number(columns = c(mean_bias, mean_empse, mean_rmse), decimals = 5) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = mean_bias, rows = measure == "PPV")
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = mean_empse, rows = measure == "PPV")
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = mean_rmse, rows = measure == "PPV")
  ) %>%
  cols_label(
    measure    = "Measure",
    mean_bias  = "Estimate",
    bias_ci    = "95% CI",
    mean_empse = "Estimate",
    empse_ci   = "95% CI",
    mean_rmse  = "Estimate",
    rmse_ci    = "95% CI"
  ) %>%
  cols_align(align = "right",  columns = measure) %>%
  cols_align(align = "center", columns = -measure) %>%
  tab_spanner(label = "Bias",  columns = c(mean_bias, bias_ci)) %>%
  tab_spanner(label = "EmpSE", columns = c(mean_empse, empse_ci)) %>%
  tab_spanner(label = "RMSE",  columns = c(mean_rmse, rmse_ci)) %>%
  tab_header(title = "Performance Measure Estimation Error") %>%
  cols_width(measure ~ px(100)) %>%
  gtsave("plots/ci_measure_table_bold.png")

# Format for printing
#summary_by_measure %>%
#  mutate(across(where(is.numeric), 
#                ~ formatC(., format = "e", digits = 3)))

# For publication table
#summary_by_measure %>%
#  mutate(across(where(is.numeric),
#                ~ formatC(., format = "e", digits = 2))) %>%
#  knitr::kable()




# Summary by model
summary_by_model <- perf_table %>%
  group_by(model) %>%
  summarise(
    mean_bias    = round(mean(bias), 4),
    mean_mc_var  = round(mean(mc_var), 6),
    mean_rmse    = round(mean(rmse), 4),
    .groups = "drop"
  )

print(summary_by_model)
saveRDS(summary_by_model, "results/summary_by_model.rds")
write.csv(summary_by_model, "results/summary_by_model.csv", row.names = FALSE)

# summary by model with ci
ci_table_model <- perf_table %>%
  group_by(model) %>%
  summarise(
    # Bias
    mean_bias     = mean(bias),
    mcse_bias     = mean(mcse_bias),
    ci_lower      = mean(bias) - 1.96 * mean(mcse_bias),
    ci_upper      = mean(bias) + 1.96 * mean(mcse_bias),
    max_bias      = max(abs(bias)),
    min_bias      = min(abs(bias)),
    
    # EmpSE (sqrt of MC variance)
    mean_empse     = mean(sqrt(mc_var)),
    mcse_empse     = mean(sqrt(mc_var)) / sqrt(2 * (1000 - 1)),
    empse_ci_lower = mean(sqrt(mc_var)) - 1.96 * (mean(sqrt(mc_var)) / sqrt(2 * (1000 - 1))),
    empse_ci_upper = mean(sqrt(mc_var)) + 1.96 * (mean(sqrt(mc_var)) / sqrt(2 * (1000 - 1))),
    max_empse      = max(sqrt(mc_var)),
    min_empse      = min(sqrt(mc_var)),
    
    #RMSE
    mean_rmse     = mean(rmse),
    mcse_rmse     = mean(mcse_rmse),
    rmse_ci_lower = mean(rmse) - 1.96 * mean(mcse_rmse),
    rmse_ci_upper = mean(rmse) + 1.96 * mean(mcse_rmse),
    max_rmse      = max(rmse),
    min_rmse      = min(rmse),
    .groups       = "drop"
  )

print(ci_table_model)
saveRDS(ci_table_model, "results/ci_table_model.rds")
write.csv(ci_table_model, "results/ci_table_model.csv", row.names = FALSE)

# summary by model with ci and heatmap
ci_table_model_heatmap <- 
  ci_table_model %>%
  dplyr::select(model, mean_bias, ci_lower, ci_upper, mean_empse,
         empse_ci_lower, empse_ci_upper, mean_rmse, 
         rmse_ci_lower, rmse_ci_upper) %>%
  mutate(model = case_when(
    model == "log"  ~ "Logistic Regression",
    model == "svm"  ~ "SVM",
    model == "tree" ~ "Decision Tree",
    model == "rf"   ~ "Random Forest",
    model == "xgb"  ~ "XGBoost",
    model == "lgb"  ~ "LightGBM",
    model == "nb"   ~ "Naive Bayes",
    model == "knn"  ~ "KNN"
  )) %>%
  mutate(model = factor(model, levels = c(
    "Logistic Regression",
    "SVM",
    "Decision Tree",
    "Random Forest",
    "XGBoost",
    "LightGBM",
    "Naive Bayes",
    "KNN"
  ))) %>%
  arrange(model) %>%
  gt() %>%
  fmt_number(decimals = 5) %>%
  fmt_scientific(
    columns = mean_bias,
    rows    = model == "XGBoost",
    decimals = 2,
    exp_style = "e"
  ) %>%
  # Color bias column — higher absolute bias = darker red
  data_color(
    columns = mean_bias,
    fn = scales::col_numeric(
      palette = c("white", "tomato3"),
      domain  = NULL
    )
  ) %>%
  data_color(
    columns = mean_empse,
    fn = scales::col_numeric(
      palette = c("white", "tomato3"),
      domain  = NULL
    )
  ) %>%
  # Color RMSE column
  data_color(
    columns = mean_rmse,
    fn = scales::col_numeric(
      palette = c("white", "tomato3"),
      domain  = NULL
    )
  ) %>%
  cols_align(align = "left", columns = model) %>%
  cols_label(
    model          = "Model",
    mean_bias      = "Mean Bias",
    ci_lower       = "95% CI Lower",
    ci_upper       = "95% CI Upper",
    mean_empse     = "Mean EmpSE",
    empse_ci_lower = "95% CI Lower",
    empse_ci_upper = "95% CI Upper",
    mean_rmse      = "RMSE",
    rmse_ci_lower  = "95% CI Lower",
    rmse_ci_upper  = "95% CI Upper"
  ) %>%
  tab_spanner(label = "Bias", columns = c(mean_bias, ci_lower, ci_upper)) %>%
  tab_spanner(label = "EmpSE", columns = c(mean_empse, empse_ci_lower, empse_ci_upper)) %>%
  tab_spanner(label = "RMSE", columns = c(mean_rmse, rmse_ci_lower, rmse_ci_upper)) %>%
  tab_header(title = "Algorithm Estimation Error") %>%
  gtsave("plots/ci_model_table.png")


# summary by model with ci and bold
# Identify worst performers
# Identify worst performers by name not index
worst_bias_model  <- ci_table_model$model[which.max(abs(ci_table_model$mean_bias))]
worst_empse_model <- ci_table_model$model[which.max(ci_table_model$mean_empse)]
worst_rmse_model  <- ci_table_model$model[which.max(ci_table_model$mean_rmse)]
cat("Worst bias:", worst_bias_model, "\n")
cat("Worst empse:", worst_empse_model, "\n")
cat("Worst rmse:", worst_rmse_model, "\n")

ci_table_bold <- 
  ci_table_model %>%
  dplyr::select(model, mean_bias, ci_lower, ci_upper, mean_empse,
                empse_ci_lower, empse_ci_upper, mean_rmse, 
                rmse_ci_lower, rmse_ci_upper) %>%  
  mutate(
    model = case_when(
      model == "log"  ~ "Logistic Regression",
      model == "svm"  ~ "SVM",
      model == "tree" ~ "Decision Tree",
      model == "rf"   ~ "Random Forest",
      model == "xgb"  ~ "XGBoost",
      model == "lgb"  ~ "LightGBM",
      model == "nb"   ~ "Naive Bayes",
      model == "knn"  ~ "KNN"
    ),
    bias_ci  = paste0("(", round(ci_lower, 5), ", ", format(round(ci_upper, 5), scientific=FALSE), ")"),
    empse_ci = paste0("(", round(empse_ci_lower, 5), ", ", round(empse_ci_upper, 5), ")"),
    rmse_ci  = paste0("(", round(rmse_ci_lower, 5), ", ", round(rmse_ci_upper, 5), ")")
  ) %>%
  mutate(model = factor(model, levels = c(
    "Logistic Regression",
    "SVM",
    "Decision Tree",
    "Random Forest",
    "XGBoost",
    "LightGBM",
    "Naive Bayes",
    "KNN"
  ))) %>%
  arrange(model) %>%
  dplyr::select(model, mean_bias, bias_ci, mean_empse, empse_ci, mean_rmse, rmse_ci) %>%
  gt() %>%
  fmt_number(columns = c(mean_bias, mean_empse, mean_rmse), decimals = 5) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = mean_bias, rows = model == "Logistic Regression")
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = mean_empse, rows = model == "KNN")
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = mean_rmse, rows = model == "KNN")
  ) %>%
  cols_align(align = "right",   columns = model) %>%
  cols_align(align = "center", columns = -model) %>%
  cols_label(
    model      = "Model",
    mean_bias  = "Estimate",
    bias_ci    = "95% CI",
    mean_empse = "Estimate",
    empse_ci   = "95% CI",
    mean_rmse  = "Estimate",
    rmse_ci    = "95% CI"
  ) %>%
  tab_spanner(label = "Bias",  columns = c(mean_bias, bias_ci)) %>%
  tab_spanner(label = "EmpSE", columns = c(mean_empse, empse_ci)) %>%
  tab_spanner(label = "RMSE",  columns = c(mean_rmse, rmse_ci)) %>%
  tab_header(title = "Algorithm Estimation Error") %>%
  gtsave("plots/ci_model_table_bold.png")




# Wide format
bias_wide <- perf_table %>%
  dplyr::select(model, measure, bias) %>%
  pivot_wider(names_from = measure, values_from = bias)

mcvar_wide <- perf_table %>%
  dplyr::select(model, measure, mc_var) %>%
  pivot_wider(names_from = measure, values_from = mc_var)

rmse_wide <- perf_table %>%
  dplyr::select(model, measure, rmse) %>%
  pivot_wider(names_from = measure, values_from = rmse)

print(bias_wide)
print(mcvar_wide)
print(rmse_wide)

# Save wide tables
write.csv(bias_wide, "results/bias_wide.csv", row.names = FALSE)
write.csv(bias_wide, "results/mcvar_wide.csv", row.names = FALSE)
write.csv(rmse_wide, "results/rmse_wide.csv", row.names = FALSE)


# ── Correlation between bias and true estimand value ──────────────────────────
p_bias_vs_true <- ggplot(perf_table, 
                         aes(x = true_val, y = bias, color = model)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "grey50", linewidth = 0.7) +
  facet_wrap(~ measure, scales = "free") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title    = "Bias vs True Estimand Value by Performance Measure",
    subtitle = "Dashed line = zero bias reference",
    x        = "True estimand value",
    y        = "Bias (estimated - true)",
    color    = "Algorithm"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text    = element_text(face = "bold")
  )

ggsave("plots/bias_vs_true.png", p_bias_vs_true,
       width = 14, height = 10, dpi = 300)




# MCSE Table (Morris et al. recommendation) 
# MCSE of bias = SD of bias estimates / sqrt(n_sim)
# MCSE of RMSE = RMSE / sqrt(2 * (n_sim - 1))

mcse_table <- perf_table %>%
  mutate(
    bias_reliable = abs(bias) > 2 * mcse_bias
  )

# ── Summary collapsed across models (one row per measure) ─────────────────────
mcse_summary <- mcse_table %>%
  group_by(measure) %>%
  summarise(
    mean_mcse_bias    = mean(mcse_bias),
    max_mcse_bias     = max(mcse_bias),
    mean_mcse_empse   = mean(mcse_empse),
    max_mcse_empse    = max(mcse_empse),
    mean_mcse_rmse    = mean(mcse_rmse),
    max_mcse_rmse     = max(mcse_rmse),
    n_reliable        = sum(bias_reliable),
    pct_reliable      = round(mean(bias_reliable) * 100, 1),
    .groups           = "drop"
  )



print(mcse_summary)
saveRDS(mcse_table,    "results/mcse_table.rds")
saveRDS(mcse_summary,  "results/mcse_summary.rds")
write.csv(mcse_table,   "results/mcse_table.csv",   row.names = FALSE)
write.csv(mcse_summary, "results/mcse_summary.csv", row.names = FALSE)



# MCSE check
# MCSE check table
mcse_check <- perf_table %>%
  group_by(measure) %>%
  summarise(
    max_mcse_bias = max(mcse_bias),
    max_mcse_rmse = max(mcse_rmse),
    .groups = "drop"
  )
print(mcse_check)







mcse_summary_table <- mcse_summary %>%
  mutate(measure = case_when(
    measure == "accuracy"          ~ "Accuracy",
    measure == "balanced_accuracy" ~ "Balanced Accuracy",
    measure == "sensitivity"       ~ "Sensitivity",
    measure == "specificity"       ~ "Specificity",
    measure == "ppv"               ~ "PPV",
    measure == "npv"               ~ "NPV",
    measure == "f1"                ~ "F1 Score",
    measure == "mcc"               ~ "MCC",
    measure == "auc"               ~ "ROC-AUC",
    measure == "brier"             ~ "Brier Score"    
  )) %>%
  mutate(measure = factor(measure, levels = c(
    "Accuracy",
    "Balanced Accuracy",
    "Sensitivity",
    "Specificity",
    "PPV",
    "NPV",
    "F1 Score",
    "MCC",
    "ROC-AUC",
    "Brier Score"
  ))) %>%
  arrange(measure) %>%
  select(measure, 
         mean_mcse_bias, max_mcse_bias,
         mean_mcse_empse, max_mcse_empse,
         mean_mcse_rmse, max_mcse_rmse,
         n_reliable, pct_reliable) %>%
  gt() %>%
  cols_label(
    measure         = "Measure",
    mean_mcse_bias  = "Mean",
    max_mcse_bias   = "Max",
    mean_mcse_empse = "Mean",
    max_mcse_empse  = "Max",
    mean_mcse_rmse  = "Mean",
    max_mcse_rmse   = "Max",
    n_reliable      = "N Reliable",
    pct_reliable    = "% Reliable"
  ) %>%
  tab_spanner(label = "MCSE Bias",   columns = c(mean_mcse_bias,  max_mcse_bias))  %>%
  tab_spanner(label = "MCSE EmpSE",  columns = c(mean_mcse_empse, max_mcse_empse)) %>%
  tab_spanner(label = "MCSE RMSE",   columns = c(mean_mcse_rmse,  max_mcse_rmse))  %>%
  fmt_number(
    columns = c(mean_mcse_bias, max_mcse_bias,
                mean_mcse_empse, max_mcse_empse,
                mean_mcse_rmse, max_mcse_rmse),
    decimals = 5
  ) %>%
  fmt_number(columns = pct_reliable, decimals = 1) %>%
  cols_align(align = "right",  columns = measure) %>%
  cols_align(align = "center", columns = -measure) %>%
  tab_header(title = "Simulation Adequacy: Monte Carlo Standard Errors") %>%
  tab_footnote(
    footnote  = "N Reliable = number of model-metric combinations (out of 8) where bias was statistically distinguishable from zero (|bias| > 2 × MCSE)",
    locations = cells_column_labels(columns = n_reliable)
  ) %>%
  cols_width(
    measure         ~ px(130),
    mean_mcse_bias  ~ px(80),
    max_mcse_bias   ~ px(80),
    mean_mcse_empse ~ px(80),
    max_mcse_empse  ~ px(80),
    mean_mcse_rmse  ~ px(80),
    max_mcse_rmse   ~ px(80),
    n_reliable      ~ px(80),
    pct_reliable    ~ px(90)
  ) %>%
  tab_options(column_labels.font.weight = "bold") %>%
  gtsave("plots/mcse_summary_table.png")
