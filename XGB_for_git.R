library(tidyverse)
library(readxl)
library(writexl)
library(caret)
library(DescTools)
library(xgboost)
library(SHAPforxgboost)

combinedwithcar<-read_xlsx("modeldata.xlsx")

set.seed(123)

combinedwithcar<-combinedwithcar %>%
  filter(!(format(date, "%m-%d") == "06-30") & !(format(date, "%m-%d") == "07-01")) #removing the stacked dates


trainindex <- sample(nrow(combinedwithcar), 0.8 * nrow(combinedwithcar)) # separating a train and test set, 80% in train
train <- combinedwithcar[trainindex, ]
test <- combinedwithcar[-trainindex, ] 

#select predictors
predictors <- c("player_age", "player_pos", "market_val", "player_nat",
                "counter_team_country", "team_name", "season",
                "transfer_direction", "transfer_type", "country",
                "transfers_before_5", "transfer_after_5",
                "sameday_transfer", "transfer_window")

#drop na in predictors to prevent different lengths late
train <- train %>% filter(complete.cases(across(all_of(predictors))))
test  <- test %>%  filter(complete.cases(across(all_of(predictors))))

#empty for results
rsquaredxgb <- data.frame(car = NA, rsq = NA, rmse=NA, mae=NA)


#5 fold cv
ctrl <- trainControl(method = "cv", number = 5)

#limited tune grid
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(4, 6),
  eta = c(0.05, 0.1),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)



#loop over different cars
for (i in c(45:58)) {
  start_time <- Sys.time()#track time per model
  
  target <- colnames(combinedwithcar)[i]
  rsquaredxgb[i - 44, 1] <- target
  
  fml <- as.formula(paste(target, "~", paste(predictors, collapse = "+"))) #formula model is trained on
  
  #caret training structure
  model <- train(
    fml,
    data = train,
    method = "xgbTree",
    trControl = ctrl,
    na.action = na.omit, #omit row if there i an na in the predictor, shouldnt be any as we filter for it
    verbose = FALSE,
    tuneGrid = xgb_grid #custom grid
  )
  
  #compute and store evaluation metrics
  preds <- predict(model, newdata = test)
  actuals <- test[[target]]
  valid <- !is.na(preds) & !is.na(actuals)
  
  r2 <- 1 - sum((actuals[valid] - preds[valid])^2) / sum((actuals[valid] - mean(actuals[valid]))^2)
  rmse <- sqrt(mean((actuals[valid] - preds[valid])^2))
  mae  <- mean(abs(actuals[valid] - preds[valid]))
  
  rsquaredxgb[i - 44, 2] <- r2
  rsquaredxgb[i - 44, 3] <- rmse
  rsquaredxgb[i - 44, 4] <- mae
  
  print(Sys.time() - start_time)
  
  #extracting best model
  xgb_model <- model$finalModel
  
  all_data <- rbind(train, test)
  full_matrix <- model.matrix(fml, data = all_data)[, -1] #matrix for shap
  train_matrix <- full_matrix[1:nrow(train), ]
  test_matrix  <- full_matrix[(nrow(train) + 1):nrow(full_matrix), ]
  
  #predict with shap values generated during
  shap_values <- predict(xgb_model, newdata = test_matrix, predcontrib = TRUE)
  shap_contribs <- shap_values[, -ncol(shap_values)]  
  
  shap_df <- as.data.frame(shap_contribs)
  shap_df$obs_id <- 1:nrow(shap_df)
  shap_df$target <- target
  
  shap_results[[target]] <- shap_df
}




#shaps from list to long df
all_shaps <- do.call(rbind, shap_results)

#turn one hot cols into one
group_vars <- c("player_pos", "player_nat", "team_name", "counter_team_country", "country")
for (prefix in group_vars) {
  matching_cols <- grep(paste0("^", prefix), names(all_shaps), value = TRUE)
  
  if (length(matching_cols) > 0) {
    #collapse dummies
    all_shaps[[prefix]] <- rowSums(all_shaps[, matching_cols], na.rm = TRUE)
    
    #drop dummies
    all_shaps <- all_shaps %>%
      select(-all_of(matching_cols))
  }
}


#calculate mean per feature per model
shap_summary <- all_shaps %>%
  select(-obs_id) %>%
  pivot_longer(-target, names_to = "feature", values_to = "shap_value") %>%
  group_by(target, feature) %>%
  summarise(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE), .groups = "drop")

