library(tidyverse)
library(ggplot2)
library(patchwork)  # for combining plots
library(ggplot2)
library(dplyr)


setwd(r"(C:\Users\emmad\Documents\Thesis)")

results <- readRDS("results/simulation_results.rds")
true_estimands <- readRDS("results/true_estimands.rds")
perf_table <- readRDS("results/performance_table.rds")

models  <- c("log", "svm", "tree", "rf", "xgb", "lgb", "nb", "knn")
measures <- c("accuracy", "balanced_accuracy", "sensitivity", "specificity",
              "ppv", "npv", "f1", "mcc", "auc", "brier")


# Helper: convert results array to long data frame 
results_long <- expand.grid(
  sim     = 1:dim(results)[1],
  measure = measures,
  model   = models,
  stringsAsFactors = FALSE
) %>%
  mutate(value = as.vector(results))

# Add true estimand values
true_long <- true_estimands %>%
  rownames_to_column("model") %>%
  pivot_longer(-model, names_to = "measure", values_to = "true_val")

results_long <- results_long %>%
  left_join(true_long, by = c("model", "measure"))

# Density plots — distribution of each measure across simulations 
plot_densities <- function(measure_name){
  
  df <- results_long %>% filter(measure == measure_name)
  true_df <- df %>% distinct(model, true_val)
  
  ggplot(df, aes(x = value, fill = model, color = model)) +
    geom_density(alpha = 0.3) +
    geom_vline(data = true_df,
               aes(xintercept = true_val, color = model),
               linetype = "dashed", linewidth = 0.8) +
    facet_wrap(~ model, scales = "free_y", ncol = 4) +
    labs(
      title    = paste("Distribution of", toupper(measure_name), "across simulations"),
      subtitle = "Dashed line = true estimand",
      x        = measure_name,
      y        = "Density"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title      = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle   = element_text(hjust = 0.5),
      strip.text      = element_text(face = "bold")
    )
}

# Save density plot for each measure
for(k in measures){
  p <- plot_densities(k)
  ggsave(
    filename = paste0("plots/distributions/density_", k, ".png"),
    plot     = p,
    width    = 14, height = 8, dpi = 300
  )
}

# Boxplots — distribution across models for each measure 
plot_boxplots <- function(measure_name){
  
  df <- results_long %>% filter(measure == measure_name)
  true_df <- df %>% distinct(model, true_val)
  
  ggplot(df, aes(x = model, y = value, fill = model)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    geom_point(data  = true_df,
               aes(x = model, y = true_val),
               shape = 18, size = 4, color = "black") +
    labs(
      title    = paste("Distribution of", toupper(measure_name), "by model"),
      subtitle = "Diamond = true estimand",
      x        = "Model",
      y        = measure_name
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title      = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle   = element_text(hjust = 0.5),
      axis.text.x     = element_text(angle = 45, hjust = 1)
    )
}

for(k in measures){
  p <- plot_boxplots(k)
  ggsave(
    filename = paste0("plots/distributions/boxplot_", k, ".png"),
    plot     = p,
    width    = 10, height = 6, dpi = 300
  )
}

# Bias plot — bias per model per measure 
plot_bias <- function(measure_name){
  
  df <- perf_table %>% filter(measure == measure_name)
  
  ggplot(df, aes(x = reorder(model, bias), y = bias, fill = bias > 0)) +
    geom_col(alpha = 0.8) +
    geom_errorbar(
      aes(ymin = bias - 1.96 * mcse_bias,
          ymax = bias + 1.96 * mcse_bias),
      width = 0.3
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_fill_manual(values = c("TRUE" = "tomato3", "FALSE" = "steelblue"),
                      labels = c("TRUE" = "Positive bias", "FALSE" = "Negative bias")) +
    labs(
      title    = paste("Bias of", toupper(measure_name), "by model"),
      subtitle = "Error bars = 95% MCSE interval",
      x        = "Model",
      y        = "Bias",
      fill     = ""
    ) +
    theme_bw() +
    theme(
      plot.title      = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle   = element_text(hjust = 0.5),
      axis.text.x     = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

for(k in measures){
  p <- plot_bias(k)
  ggsave(
    filename = paste0("plots/bias/bias_", k, ".png"),
    plot     = p,
    width    = 10, height = 6, dpi = 300
  )
}

# Convergence plots — running mean across simulations 
plot_convergence <- function(measure_name, model_name){
  
  sim_vals <- results[, measure_name, model_name]
  true_val <- true_estimands[model_name, measure_name]
  
  running_mean <- cumsum(sim_vals) / seq_along(sim_vals)
  
  df <- data.frame(
    sim          = 1:length(sim_vals),
    running_mean = running_mean,
    true_val     = true_val
  )
  
  ggplot(df, aes(x = sim, y = running_mean)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_hline(aes(yintercept = true_val),
               linetype = "dashed", color = "tomato3", linewidth = 0.8) +
    labs(
      title    = paste("Convergence —", toupper(model_name), "|", toupper(measure_name)),
      subtitle = "Dashed line = true estimand",
      x        = "Simulation",
      y        = paste("Running mean of", measure_name)
    ) +
    theme_bw() +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
}


for(m in models){
  # AUC convergence
  p_auc <- plot_convergence("auc", m)
  ggsave(
    filename = paste0("plots/convergence/auc/convergence_auc_", m, ".png"),
    plot     = p_auc,
    width    = 8, height = 5, dpi = 300
  )
  
  # Brier score convergence
  p_brier <- plot_convergence("brier", m)
  ggsave(
    filename = paste0("plots/convergence/brier/convergence_brier_", m, ".png"),
    plot     = p_brier,
    width    = 8, height = 5, dpi = 300
  )
}

# Combined convergence plot — all models for one measure 
plot_convergence_all_models <- function(measure_name){
  
  df_list <- lapply(models, function(m){
    sim_vals     <- results[, measure_name, m]
    running_mean <- cumsum(sim_vals) / seq_along(sim_vals)
    data.frame(
      sim          = 1:length(sim_vals),
      running_mean = running_mean,
      true_val     = true_estimands[m, measure_name],
      model        = m
    )
  })
  
  df <- do.call(rbind, df_list)
  
  ggplot(df, aes(x = sim, y = running_mean, color = model)) +
    geom_line(linewidth = 0.8) +
    geom_hline(aes(yintercept = true_val, color = model),
               linetype = "dashed", linewidth = 0.5) +
    facet_wrap(~ model, ncol = 4, scales = "free_y") +
    labs(
      title    = paste("Convergence of", toupper(measure_name), "— all models"),
      subtitle = "Dashed line = true estimand",
      x        = "Simulation",
      y        = paste("Running mean of", measure_name)
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title      = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle   = element_text(hjust = 0.5),
      strip.text      = element_text(face = "bold")
    )
}

# Save combined convergence for all measures
for(k in measures){
  p <- plot_convergence_all_models(k)
  ggsave(
    filename = paste0("plots/convergence/convergence_all_", k, ".png"),
    plot     = p,
    width    = 14, height = 8, dpi = 300
  )
}



#bias by metric
bias_metric_df <- perf_table %>%
  group_by(measure) %>%
  summarise(
    mean_bias  = mean(bias),
    mean_mcse  = mean(mcse_bias),
    ci_lower   = mean(bias) - 1.96 * mean(mcse_bias),
    ci_upper   = mean(bias) + 1.96 * mean(mcse_bias),
    .groups    = "drop"
  ) %>%
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
    significant = ci_lower > 0 | ci_upper < 0
  )

ggplot(bias_metric_df, 
       aes(x = reorder(measure, mean_bias), 
           y = mean_bias,
           color = significant)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "gray60"),
    labels = c("TRUE" = "Distinguishable from 0", 
               "FALSE" = "Not distinguishable from 0"),
    name   = ""
  ) +
  coord_flip() +
  labs(
    title    = "Estimated Bias by Performance Metric",
    subtitle = "Error bars = 95% Monte Carlo CI (± 1.96 × MCSE)",
    x        = NULL,
    y        = "Estimated Bias"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("plots/bias_by_metric.png", width = 8, height = 6, dpi = 300)



#bias by metric
bias_metric_df <- perf_table %>%
  group_by(measure) %>%
  summarise(
    mean_bias  = mean(bias),
    mean_mcse  = mean(mcse_bias),
    ci_lower   = mean(bias) - 1.96 * mean(mcse_bias),
    ci_upper   = mean(bias) + 1.96 * mean(mcse_bias),
    .groups    = "drop"
  ) %>%
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
    significant = ci_lower > 0 | ci_upper < 0
  )

ggplot(bias_metric_df, 
       aes(x = reorder(measure, mean_bias), 
           y = mean_bias,
           color = significant)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "gray60"),
    labels = c("TRUE" = "Distinguishable from 0", 
               "FALSE" = "Not distinguishable from 0"),
    name   = ""
  ) +
  coord_flip() +
  labs(
    title    = "Estimated Bias by Performance Measure",
    subtitle = "Error bars = 95% Monte Carlo CI (± 1.96 × MCSE)",
    x        = NULL,
    y        = "Estimated Bias"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("plots/bias_by_metric.png", width = 8, height = 6, dpi = 300)



#EmpSE by metric
empse_metric_df <- perf_table %>%
  group_by(measure) %>%
  summarise(
    mean_empse  = mean(emp_se),
    mean_mcse  = mean(mcse_empse),
    ci_lower   = mean(emp_se) - 1.96 * mean(mcse_empse),
    ci_upper   = mean(emp_se) + 1.96 * mean(mcse_empse),
    .groups    = "drop"
  ) %>%
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
    ))

ggplot(empse_metric_df, 
       aes(x = reorder(measure, mean_empse), 
           y = mean_empse)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, color = "steelblue") +
  geom_point(size = 3,color = "steelblue") +
  coord_flip() +
  labs(
    title    = "Estimated Empirical Standard Error by Performance Measure",
    subtitle = "Error bars = 95% Monte Carlo CI (± 1.96 × MCSE)",
    x        = NULL,
    y        = "Estimated Empirical Standard Error"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("plots/empse_by_metric.png", width = 8, height = 6, dpi = 300)




#RMSE by metric
rmse_metric_df <- perf_table %>%
  group_by(measure) %>%
  summarise(
    mean_rmse  = mean(rmse),
    mean_mcse  = mean(mcse_rmse),
    ci_lower   = mean(rmse) - 1.96 * mean(mcse_rmse),
    ci_upper   = mean(rmse) + 1.96 * mean(mcse_rmse),
    .groups    = "drop"
  ) %>%
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
    ))

ggplot(rmse_metric_df, 
       aes(x = reorder(measure, mean_rmse), 
           y = mean_rmse)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, color = "steelblue") +
  geom_point(size = 3,color = "steelblue") +
  coord_flip() +
  labs(
    title    = "Estimated Root Mean Squared Error by Performance Measure",
    subtitle = "Error bars = 95% Monte Carlo CI (± 1.96 × MCSE)",
    x        = NULL,
    y        = "Estimated Root Mean Squared Error"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("plots/rmse_by_metric.png", width = 8, height = 6, dpi = 300)





# Bias by model algorithm
bias_model_df <- perf_table %>%
  group_by(model) %>%
  summarise(
    mean_bias  = mean(bias),
    mean_mcse  = mean(mcse_bias),
    ci_lower   = mean(bias) - 1.96 * mean(mcse_bias),
    ci_upper   = mean(bias) + 1.96 * mean(mcse_bias),
    .groups    = "drop"
  ) %>%
  mutate(
    model = case_when(
      model == "log"  ~ "Logistic Regression",
      model == "svm"  ~ "SVM",
      model == "tree" ~ "Decision Tree",
      model == "rf"   ~ "Random Forest",
      model == "xgb"  ~ "XGBoost",
      model == "lgb"  ~ "LightGBM",
      model == "nb"   ~ "Naïve Bayes",
      model == "knn"  ~ "KNN"
    ),
    significant = ci_lower > 0 | ci_upper < 0
  )

ggplot(bias_model_df,
       aes(x = reorder(model, mean_bias),
           y = mean_bias,
           color = significant)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "gray60"),
    labels = c("TRUE" = "Distinguishable from 0",
               "FALSE" = "Not distinguishable from 0"),
    name   = ""
  ) +
  coord_flip() +
  labs(
    title    = "Estimated Bias by Algorithm",
    subtitle = "Error bars = 95% Monte Carlo CI (± 1.96 × MCSE)",
    x        = NULL,
    y        = "Estimated Bias"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("plots/bias_by_model.png", width = 8, height = 6, dpi = 300)



# empse by model algorithm
empse_model_df <- perf_table %>%
  group_by(model) %>%
  summarise(
    mean_empse = mean(emp_se),
    mean_mcse  = mean(mcse_empse),
    ci_lower   = mean(emp_se) - 1.96 * mean(mcse_empse),
    ci_upper   = mean(emp_se) + 1.96 * mean(mcse_empse),
    .groups    = "drop"
  ) %>%
  mutate(
    model = case_when(
      model == "log"  ~ "Logistic Regression",
      model == "svm"  ~ "SVM",
      model == "tree" ~ "Decision Tree",
      model == "rf"   ~ "Random Forest",
      model == "xgb"  ~ "XGBoost",
      model == "lgb"  ~ "LightGBM",
      model == "nb"   ~ "Naïve Bayes",
      model == "knn"  ~ "KNN"
    ),
  )

ggplot(empse_model_df,
       aes(x = reorder(model, mean_empse),
           y = mean_empse,t)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, color="steelblue") +
  geom_point(size = 3,color="steelblue") +
  coord_flip() +
  labs(
    title    = "Estimated Empirical Standard Error by Algorithm",
    subtitle = "Error bars = 95% Monte Carlo CI (± 1.96 × MCSE)",
    x        = NULL,
    y        = "Estimated Empirical Standard Error"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("plots/empse_by_model.png", width = 8, height = 6, dpi = 300)



# rmse by model algorithm
rmse_model_df <- perf_table %>%
  group_by(model) %>%
  summarise(
    mean_rmse  = mean(rmse),
    mean_mcse  = mean(mcse_rmse),
    ci_lower   = mean(rmse) - 1.96 * mean(mcse_rmse),
    ci_upper   = mean(rmse) + 1.96 * mean(mcse_rmse),
    .groups    = "drop"
  ) %>%
  mutate(
    model = case_when(
      model == "log"  ~ "Logistic Regression",
      model == "svm"  ~ "SVM",
      model == "tree" ~ "Decision Tree",
      model == "rf"   ~ "Random Forest",
      model == "xgb"  ~ "XGBoost",
      model == "lgb"  ~ "LightGBM",
      model == "nb"   ~ "Naïve Bayes",
      model == "knn"  ~ "KNN"
    )
  )

ggplot(rmse_model_df,
       aes(x = reorder(model, mean_rmse),
           y = mean_rmse,t)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, color="steelblue") +
  geom_point(size = 3,color="steelblue") +
  coord_flip() +
  labs(
    title    = "Estimated Root Mean Squared Error by Algorithm",
    subtitle = "Error bars = 95% Monte Carlo CI (± 1.96 × MCSE)",
    x        = NULL,
    y        = "Estimated Root Mean Squared Error"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("plots/rmse_by_model.png", width = 8, height = 6, dpi = 300)
