library(tidyverse)
library(readxl)
library(writexl)
library(caret)

combinedwithcar<-read_xlsx("modeldata.xlsx")

combinedwithcar<-combinedwithcar%>%
  mutate(player_pos=as.factor(player_pos),
         player_nat=as.factor(player_nat),
         counter_team_country=as.factor(counter_team_country),
         counter_team_name=as.factor(counter_team_name),
         team_name=as.factor(team_name),
         transfer_window=as.factor(transfer_window),
         transfer_direction=as.factor(transfer_direction),
         country=as.factor(country),
         transfer_type=as.factor(transfer_type)
  )

set.seed(123)

combinedwithcar<-combinedwithcar %>%
  filter(!(format(date, "%m-%d") == "06-30") & !(format(date, "%m-%d") == "07-01")) #for nodate dataset


train_index <- sample(nrow(combinedwithcar), 0.8 * nrow(combinedwithcar)) #random index 0.8 times the size of full data
train <- combinedwithcar[train_index, ]
test <- combinedwithcar[-train_index, ]



#empty df to safe stuff in
rsquaredlm <- data.frame(car = NA, rsq = NA, rmse=NA, mae=NA)

#independent variables
predictors <- c("player_age", "player_pos", "market_val", "player_nat",
                "counter_team_country", "team_name", "season",
                "transfer_direction", "transfer_type", "country",
                "transfers_before_5", "transfer_after_5",
                "sameday_transfer", "transfer_window")
#for cross validation
ctrl <- trainControl(method = "cv", number = 5)

shap_lm_results <- list()

coef_list <- list()

for (i in 45:58) {
  start_time <- Sys.time() #track time
  
  target <- colnames(combinedwithcar)[i] #extract dep var
  
  # Remove rows with missing values in target or predictors
  test_clean <- test %>%
    filter(complete.cases(across(c(target, predictors)))) #only complete cases to prevent errors
  
  rsquaredlm[i - 44, "target"] <- target #first kol the rownames of car window
  
  fml <- as.formula(paste(target, "~", paste(predictors, collapse = "+"))) #formula we train the model on
  
  # use caret to train, similar framework as more complex models
  model_lm <- train(
    fml,
    data = train,
    method = "lm",
    trControl = ctrl,
    na.action = na.omit
  )
  
  #best model for each loop
  model_summary <- summary(model_lm$finalModel)
  coef_table <- as.data.frame(coef(model_summary)) #extract coefs
  
  #remove intercept, if its there
  if (nrow(coef_table) > 1) {
    coef_table <- coef_table[-1, , drop = FALSE]
  } else {
    next
  }
  
  #create table with coefs and summary stats
  coef_table$feature <- rownames(coef_table)
  coef_table$target <- target
  colnames(coef_table) <- c("estimate", "std_error", "t_value", "p_value", "feature", "target")
  
  coef_list[[target]] <- coef_table
  
  #model performance out of sample
  preds <- predict(model_lm, newdata = test_clean)
  actuals <- test_clean[[target]]
  valid <- !is.na(preds) & !is.na(actuals)
  
  r2 <- 1 - sum((actuals[valid] - preds[valid])^2) / sum((actuals[valid] - mean(actuals[valid]))^2)
  rmse <- sqrt(mean((actuals[valid] - preds[valid])^2))
  mae <- mean(abs(actuals[valid] - preds[valid]))
  
  rsquaredlm[i - 44, "rsq"] <- r2
  rsquaredlm[i - 44, "rmse"] <- rmse
  rsquaredlm[i - 44, "mae"] <- mae
  
  print(paste("R²:", round(r2, 4)))
  print(Sys.time() - start_time)
  
  
  test_matrix <- model.matrix(fml, data = test_clean)[, -1] #create dummies and removes intercept
  coefs <- coef(model_lm$finalModel)[-1]
  
  contribs <- sweep(test_matrix, 2, coefs, `*`) #apply function * across cols, multiply coef with value
  shap_df <- as.data.frame(contribs)
  shap_df$obs_id <- 1:nrow(shap_df)
  shap_df$target <- target
  
  shap_lm_results[[target]] <- shap_df
  
}

# combine all SHAP values
all_shaps_lm <- do.call(rbind, shap_lm_results)

#collapse one hot encoded column so we only have one per category instead of many
group_vars <- c("player_pos", "player_nat", "team_name", "counter_team_country", "country")
for (prefix in group_vars) {
  matching_cols <- grep(paste0("^", prefix), names(all_shaps_lm), value = TRUE)
  if (length(matching_cols) > 0) {
    all_shaps_lm[[prefix]] <- rowSums(all_shaps_lm[, matching_cols], na.rm = TRUE)
    all_shaps_lm <- all_shaps_lm %>%
      select(-all_of(matching_cols))
  }
}

#create "average absolute SHAP values" for comparison
shap_summary_lm <- all_shaps_lm %>%
  select(-obs_id) %>%
  pivot_longer(-target, names_to = "feature", values_to = "contribution") %>%
  group_by(target, feature) %>%
  summarise(mean_abs_contribution = mean(abs(contribution), na.rm = TRUE), .groups = "drop")

#all coefs together
all_lm_coefs <- do.call(rbind, coef_list)
