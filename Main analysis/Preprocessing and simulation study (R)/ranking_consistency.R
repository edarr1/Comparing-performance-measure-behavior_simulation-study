library(corrplot)
library(ggplot2)
library(tidyverse)
library(gt)


setwd(r"(C:\Users\emmad\Documents\Thesis)")

results <- readRDS("results/simulation_results.rds")
true_estimands <- readRDS("results/true_estimands.rds")

models  <- c("log", "svm", "tree", "rf", "xgb", "lgb", "nb", "knn")
measures <- c("accuracy", "balanced_accuracy", "sensitivity", "specificity",
              "ppv", "npv", "f1", "mcc", "auc", "brier")



# Ranking consistency using between-simulation correlation
compute_ranking_stability <- function(results, models, measures){
  
  stability_list <- list()
  
  for(k in measures){
    
    sim_matrix <- results[, k, models]
    
    if(k == "brier"){
      rank_matrix <- t(apply(sim_matrix, 1, 
                             function(x) rank(x, ties.method = "average", 
                                              na.last = "keep")))
    } else {
      rank_matrix <- t(apply(sim_matrix, 1, 
                             function(x) rank(-x, ties.method = "average", 
                                              na.last = "keep")))
    }
    colnames(rank_matrix) <- models
    
    mean_ranks <- colMeans(rank_matrix, na.rm = TRUE)
    rank_sds   <- apply(rank_matrix, 2, sd, na.rm = TRUE)
    
    sim_cor <- cor(t(rank_matrix), method = "spearman", 
                   use = "pairwise.complete.obs")
    
    # Extract upper triangle values
    upper_cors <- sim_cor[upper.tri(sim_cor)]
    upper_cors <- upper_cors[!is.na(upper_cors)]
    
    # Mean, SD, SE, and 95% CI
    mean_sim_cor <- mean(upper_cors)
    sd_sim_cor   <- sd(upper_cors)
    n_pairs      <- length(upper_cors)
    se_sim_cor   <- sd_sim_cor / sqrt(n_pairs)
    ci_lower     <- mean_sim_cor - 1.96 * se_sim_cor
    ci_upper     <- mean_sim_cor + 1.96 * se_sim_cor
    
    top_model_freq <- apply(rank_matrix, 2, function(x) mean(x == 1, na.rm = TRUE))
    
    stability_list[[k]] <- list(
      mean_ranks   = mean_ranks,
      rank_sds     = rank_sds,
      mean_sim_cor = mean_sim_cor,
      sd_sim_cor   = sd_sim_cor,
      ci_lower     = ci_lower,
      ci_upper     = ci_upper,
      n_pairs      = n_pairs,
      top_model_freq = top_model_freq
    )
  }
  
  return(stability_list)
}


stability_results <- compute_ranking_stability(results, models, measures)

# Summary preview
cat("\n=== Ranking Stability Summary ===\n")
for(k in measures){
  cat("\nMeasure:", k, "\n")
  cat("  Mean between-sim Spearman correlation:", 
      round(stability_results[[k]]$mean_sim_cor, 4), "\n")
  cat("  Mean ranks:\n")
  print(round(stability_results[[k]]$mean_ranks, 2))
  cat("  Rank SDs:\n")
  print(round(stability_results[[k]]$rank_sds, 3))
  cat("  Frequency ranked 1st:\n")
  print(round(stability_results[[k]]$top_model_freq, 3))
}

# Summary table
stability_summary <- data.frame(
  measure      = measures,
  mean_sim_cor = sapply(measures, function(k) stability_results[[k]]$mean_sim_cor),
  sd_sim_cor   = sapply(measures, function(k) stability_results[[k]]$sd_sim_cor),
  ci_lower     = sapply(measures, function(k) stability_results[[k]]$ci_lower),
  ci_upper     = sapply(measures, function(k) stability_results[[k]]$ci_upper),
  top_model    = sapply(measures, function(k) 
    models[which.max(stability_results[[k]]$top_model_freq)]),
  top_model_freq = sapply(measures, function(k) 
    max(stability_results[[k]]$top_model_freq))
)

print(stability_summary[, c("measure", "mean_sim_cor", "sd_sim_cor", "ci_lower", "ci_upper", "top_model_freq")])
saveRDS(stability_results, "results/stability_results.rds")
write.csv(stability_summary, "results/stability_summary.csv", row.names = FALSE)




# Mean rank heatmap — models x measures
mean_rank_df <- do.call(rbind, lapply(measures, function(k){
  data.frame(
    measure   = k,
    model     = models,
    mean_rank = stability_results[[k]]$mean_ranks
  )
}))

# Order models by overall mean rank
# Reorder by overall mean rank (best at bottom, worst at top)
model_order <- mean_rank_df %>%
  group_by(model) %>%
  summarise(overall_rank = mean(mean_rank)) %>%
  arrange(desc(overall_rank)) %>%  # worst at top, best at bottom
  pull(model)

mean_rank_df$model <- factor(mean_rank_df$model, levels = model_order)
mean_rank_df$measure <- factor(mean_rank_df$measure, levels = measures)

p_rank_heatmap <- ggplot(mean_rank_df, 
                         aes(x = measure, y = model, fill = mean_rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_rank, 1)), size = 3.5) +
  scale_fill_gradient2(
    low      = "steelblue",
    mid      = "white", 
    high     = "tomato3",
    midpoint = 4.5,
    name     = "Mean rank\n(1=best)"
  ) +
  labs(
    title    = "Mean Algorithm Rankings by Performance Measure",
    subtitle = "Lower rank = better performance",
    x        = "Performance measure",
    y        = "Algorithm"
  ) +
  theme_bw() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x  = element_text(angle = 45, hjust = 1)
  )

ggsave("plots/rank_heatmap.png", p_rank_heatmap, 
       width = 12, height = 6, dpi = 300)



# Ranking stability bar chart — mean Spearman by measure 
stability_df <- data.frame(
  measure      = measures,
  mean_spearman = sapply(measures, function(k) 
    stability_results[[k]]$mean_sim_cor)
)

p_stability <- ggplot(stability_df, 
                      aes(x = reorder(measure, mean_spearman), 
                          y = mean_spearman, fill = mean_spearman)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0.95, linetype = "dashed", 
             color = "tomato3", linewidth = 0.8) +
  scale_fill_gradient(low = "steelblue", high = "darkblue") +
  scale_y_continuous(limits = c(0.9, 1.0), oob = scales::squish) +
  labs(
    title    = "Ranking Stability by Performance Measure",
    subtitle = "Mean between-simulation Spearman correlation\nDashed line = 0.95 reference",
    x        = "Performance measure",
    y        = "Mean Spearman correlation"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5),
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

ggsave("plots/ranking_stability.png", p_stability,
       width = 10, height = 6, dpi = 300)

# Frequency ranked 1st — which model wins by each measure 
freq_df <- do.call(rbind, lapply(measures, function(k){
  data.frame(
    measure   = k,
    model     = models,
    freq_top  = stability_results[[k]]$top_model_freq
  )
})) %>% filter(freq_top > 0)  # only show models that were ever ranked 1st

p_freq_top <- ggplot(freq_df, 
                     aes(x = measure, y = freq_top, fill = model)) +
  geom_col(position = "stack", alpha = 0.85) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "Frequency Ranked Best by Performance Measure",
    subtitle = "Proportion of simulations each algorithm ranked 1st",
    x        = "Performance measure",
    y        = "Proportion of simulations",
    fill     = "Algorithm"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x   = element_text(angle = 45, hjust = 1)
  )

ggsave("plots/freq_ranked_first.png", p_freq_top,
       width = 10, height = 6, dpi = 300)




# rank table
stability_summary_table <- 
  stability_summary %>%
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
    top_model = case_when(
      top_model == "log"  ~ "LOG",
      top_model == "svm"  ~ "SVM",
      top_model == "rf"   ~ "RF",
      top_model == "tree" ~ "DT",
      top_model == "xgb"  ~ "XGB",
      top_model == "lgb"  ~ "LGBM",
      top_model == "knn"  ~ "KNN",
      top_model == "nb"   ~ "NB"
    ),
    # Create combined mean ± SD column as formatted string
    mean_sd = paste0(formatC(mean_sim_cor, digits = 5, format = "f"),
                     " ± ",
                     formatC(sd_sim_cor,   digits = 5, format = "f"))
  ) %>%
  select(measure, mean_sd, top_model, top_model_freq) %>%
  gt() %>%
  fmt_number(columns = top_model_freq, decimals = 4) %>%
  cols_label(
    measure        = "Measure",
    mean_sd        = "Mean Spearman ± SD",
    top_model      = "Top Model",
    top_model_freq = "Top Model Frequency"
  ) %>%
  cols_align(align = "right",  columns = measure) %>%
  cols_align(align = "center", columns = -measure) %>%
  tab_header(title = "Between-Simulation Ranking Correlation") %>%
  cols_width(
    measure        ~ px(130),
    mean_sd        ~ px(180),
    top_model      ~ px(100),
    top_model_freq ~ px(140)
  ) %>%
  tab_footnote(
    footnote  = "LOG = Logistic Regression, SVM = Support Vector Machine, DT = Decision Tree, RF = Random Forest",
    locations = cells_column_labels(columns = top_model)
  ) %>%
  gtsave("plots/stability_summary_table.png")
  
  
  
