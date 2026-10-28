
library(dplyr)
library(caret)
library(recipes)


setwd(r"(C:\Users\emmad\Documents\Thesis)")

dat <- readRDS("data/mymimic_sepsisfinal.rds")

#data types
dat$subject_id <- as.character(dat$subject_id)
dat$stay_id <- as.character(dat$stay_id)
dat$gender <- as.factor(dat$gender)
dat$metastatic_solid_tumor_max <- as.factor(dat$metastatic_solid_tumor_max)
dat$vent_status <- as.factor(dat$vent_status)
dat$positive_culture <- as.factor(dat$positive_culture)
dat$death_within_30_days <- as.factor(dat$death_within_30_days) 

#remove high percentage missing values
data.frame(
  column = names(dat),
  na_count = colSums(is.na(dat)),
  na_pct = round(colMeans(is.na(dat)) * 100, 2)
) %>%
  dplyr::filter(na_count > 0) %>%
  dplyr::arrange(desc(na_count))

removed_missing <- which(colnames(dat) %in% c("crp_max","height_mean"))
dat <- dat[,-removed_missing] 

#impute NA vent status as 0
dat$vent_status[is.na(dat$vent_status)] <- 0

#remove duplicate rows
any(duplicated(dat$subject_id))
dat$subject_id[duplicated(dat$subject_id) == T]
dat[dat$subject_id == "14634306","stay_id"]
dat[dat$subject_id == "16032226","stay_id"]
dat[dat$subject_id == "15573438","stay_id"]

removed_dup_stay <- which(dat$stay_id %in% c("34778420","38444734","38348864"))
dat <- dat[-removed_dup_stay,]
any(duplicated(dat$subject_id))
any(duplicated(dat$stay_id))


#remove id variables and data leakage variables
removed_ids <- which(colnames(dat) %in% c("subject_id","stay_id","los_icu","los_hospital"))
dat <- dat[,-removed_ids]            # remove ids and possible data leakage var

#split data
set.seed(123)
trainingIndex <- createDataPartition(dat$death_within_30_days, p=0.8, list=F)
train_raw <- dat[trainingIndex, ]
test_raw <- dat[-trainingIndex, ]

saveRDS(dat, "data/dat_raw.rds")
saveRDS(train_raw, "data/train_raw.rds")
saveRDS(test_raw, "data/test_raw.rds")

#imputation, dummy coding, remove nzv
rec <- recipe(death_within_30_days ~ ., data = train_raw) %>%
  step_indicate_na(all_numeric_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors())

rec_prep <- prep(rec)
train_proc <- bake(rec_prep, new_data = train_raw)
test_proc <- bake(rec_prep, new_data = test_raw)




#multicollinearity
#check for variables with 0 variance
numericvar <- train_proc[,sapply(train_proc, is.numeric)]
zero_var_cols <- names(which(apply(numericvar, 2, sd) == 0))
zero_var_cols

#pairwise correlations
cormatrix <- cor(numericvar)
#print high correlation numeric pairs
thresh <- 0.8
high_cor_idx <- which(abs(cormatrix) > thresh & abs(cormatrix) < 1, arr.ind = TRUE) # Find indices of correlations above threshold (excluding diagonal)
high_cor_idx <- high_cor_idx[high_cor_idx[,1] < high_cor_idx[,2], ] # Remove duplicate pairs (cor_matrix is symmetric)
# Create a tidy table of variable pairs and correlation
high_cor_pairs <- data.frame(
  var1 = colnames(cormatrix)[high_cor_idx[,1]],
  var2 = colnames(cormatrix)[high_cor_idx[,2]],
  correlation = cormatrix[high_cor_idx]
)
high_cor_pairs <- high_cor_pairs[order(-abs(high_cor_pairs$correlation)), ]
print(high_cor_pairs)


removed_corr <- which(colnames(train_proc) %in% 
                        c("hematocrit_min","hematocrit_max","sapsii_prob_max",
                          "bun_min","creatinine_min","wbc_min","bicarbonate_max",
                          "chloride_min","cul_anti_period"))
train_proc <- train_proc[,-removed_corr]
test_proc <- test_proc[,-removed_corr]


#Variance inflation factor
library(car)
lm_model <- lm(as.numeric(death_within_30_days) ~ ., data = train_proc)
vif_values <- car::vif(lm_model)
vif_values
#flag high VIFs
high_vif <- vif_values[vif_values > 5]  # threshold: 5 or 10
if(length(high_vif) > 0){
  print("Variables with high VIF (potential multicollinearity):")
  print(high_vif)
} else {
  print("No severe multicollinearity detected")
}
cor(train_proc[, c("sodium_max","chloride_max","sbp_mean")],
    use = "pairwise.complete.obs")


#feature selection
library(dplyr)
library(caret) 
# Separate numeric and categorical/dummy variables
numeric_vars <- dplyr::select(train_proc, where(is.numeric), -death_within_30_days)
dummy_vars <- numeric_vars %>% select_if(function(x) all(x %in% c(0,1)))
num_only_vars <- setdiff(names(numeric_vars), names(dummy_vars))
numeric_pvals <- sapply(num_only_vars, function(colname) {
  # Extract numeric vector from train_proc
  x <- numeric_vars[[colname]]
  
  # Fit ANOVA
  summary(aov(x ~ train_proc$death_within_30_days))[[1]][["Pr(>F)"]][1]
})
# Select numeric predictors with p < 0.5
numeric_keep <- names(numeric_pvals[numeric_pvals < 0.5])
numeric_keep
#chi squared
dummy_pvals <- sapply(names(dummy_vars), function(colname) {
  x <- dummy_vars[[colname]]
  tbl <- table(factor(x), train_proc$death_within_30_days)
  if(any(tbl == 0)) tbl <- tbl + 0.5  # continuity correction
  chisq.test(tbl)$p.value
})
# Keep categorical predictors with p < 0.5
cat_keep <- names(dummy_pvals[dummy_pvals < 0.5])
cat_keep


#transform and scaling
bin <- c("na_ind_inr_max",
         "vent_status_X4",
         "positive_culture_X1",
         "metastatic_solid_tumor_max_X1",
         "gender_M")
cont <- setdiff(
  names(train_proc)[sapply(train_proc, is.numeric)],
  c(bin, "death_within_30_days")
)

rec2 <- recipe(death_within_30_days ~ ., data = train_proc) %>%
  step_YeoJohnson(all_of(cont)) %>%   #transform
  step_normalize(all_of(cont))   #scale numeric predictors


rec2_prep <- prep(rec2, training = train_proc, retain = TRUE)
#train_proc_fin <- juice(rec2_prep)
test_proc_fin <- bake(rec2_prep, new_data = test_proc)
train_proc_fin <- bake(rec2_prep, new_data = train_proc)


#final assignment
train_sim <- train_proc_fin
train <- train_proc_fin
test <- test_proc_fin

#quick imbalance check
summary(train$death_within_30_days)
summary(train_sim$death_within_30_days)
summary(test$death_within_30_days)


# save
saveRDS(train, file = "data/train.rds")
saveRDS(train, file = "data/train_sim.rds")
saveRDS(test, file = "data/test.rds")




# check train data
summary(train)

