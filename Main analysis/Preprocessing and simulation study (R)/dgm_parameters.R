library(copula)
library(MASS)
library(Matrix)
library(dplyr)

setwd(r"(C:\Users\emmad\Documents\Thesis)")

# Set up data
train_sim<- readRDS("data/train.rds")
x <- dplyr::select(train_sim, -death_within_30_days)
y <- train_sim$death_within_30_days

bin <- c("na_ind_inr_max",
         "vent_status_X4",
         "positive_culture_X1",
         "metastatic_solid_tumor_max_X1",
         "gender_M")
cont <- setdiff(
  names(train_sim)[sapply(train_sim, is.numeric)],
  c(bin, "death_within_30_days")
)
bin_idx <- which(names(x) %in% bin)
cont_idx <- which(names(x) %in% cont)


# Save Marginal Parameters
# Continuous mean & SD
cont_mean <- colMeans(x[, cont_idx])
cont_sd <- apply(x[, cont_idx], 2, sd)
# Binary proportion
bin_prop <- colMeans(x[, bin_idx])


# Estimate correlation matrix
cor_matrix <- cor(x, method = "pearson")

# Check & correct positive definiteness
cor_matrix_pd <- as.matrix(nearPD(cor_matrix, corr = TRUE)$mat)


# Save parameters
dgm_params <- list(
  cont_means = cont_mean,
  cont_sds = cont_sd,
  bin_props = bin_prop,
  bin_idx = bin_idx,
  cont_idx = cont_idx,
  cor_matrix_pd = cor_matrix_pd,
  var_names = names(x),
  n_vars = ncol(x)
)

# Add min/max to dgm_params
dgm_params$cont_min <- apply(x[, cont_idx], 2, min)
dgm_params$cont_max <- apply(x[, cont_idx], 2, max)


# Save to disk
saveRDS(dgm_params, file = "data/dgm_params.rds")


# Verify
cat("Number of predictors:", dgm_params$n_vars, "\n")
cat("Continuous variables:", length(cont_idx), "\n")
cat("Binary variables:", length(bin_idx), "\n")
cat("Correlation matrix positive definite:",
    all(eigen(cor_matrix_pd)$values > 0), "\n")

