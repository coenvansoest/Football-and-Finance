library(tidyverse)
library(readxl)
library(writexl)
library(randomForest)
library(ranger)
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
         country=as.factor(country)
  )

set.seed(123)

combinedwithcar<-combinedwithcar %>%
  filter(!(format(date, "%m-%d") == "06-30") & !(format(date, "%m-%d") == "07-01")) #removing to the two dates where obs were stacked




#split data
trainindex <- sample(nrow(combinedwithcar), 0.8 * nrow(combinedwithcar))#random index of 80% length
train <- combinedwithcar[trainindex, ]
test  <- combinedwithcar[-trainindex, ]

#select the predictors (independent vars)
predictors <- c("player_age", "player_pos", "market_val", "player_nat",
                "counter_team_country", "team_name", "season",
                "transfer_direction", "transfer_type", "country",
                "transfers_before_5", "transfer_after_5",
                "sameday_transfer", "transfer_window") 

#get rid of nas so we dont have different lengths to compare later
test <- test %>% filter(complete.cases(across(all_of(predictors))))

#5 fold cross val
ctrl <- trainControl(method = "cv", number = 5)

#empty objects to safe output loop
rsquaredrf <- data.frame(car = NA, rsq = NA, rmse = NA, mae = NA)
shap_rf_results <- list()

#loop
for (i in 45:58) { #index of cars
  start_time <- Sys.time() #to keep track of how long model takes per loop
  
  target <- colnames(combinedwithcar)[i] #first col to keep track for with car we have which variable
  rsquaredrf[i - 44, 1] <- target 
  
  fml <- as.formula(paste(target, "~", paste(predictors, collapse = "+"))) #formula to train model on
  
  # to train model
  model <- train(
    fml,
    data = train,
    method = "ranger",
    trControl = ctrl,
    importance = "impurity",
    na.action = na.omit,
    num.trees = 200
  )
  
  #evaluate model performance with test data
  preds <- predict(model, newdata = test)
  actuals <- test[[target]]
  valid <- !is.na(preds) & !is.na(actuals)
  
  r2 <- 1 - sum((actuals[valid] - preds[valid])^2) / sum((actuals[valid] - mean(actuals[valid]))^2)
  rmse <- sqrt(mean((actuals[valid] - preds[valid])^2))
  mae  <- mean(abs(actuals[valid] - preds[valid]))
  
  rsquaredrf[i - 44, 2:4] <- c(r2, rmse, mae)
  
  #extracting shap values
  #changing data so we can extract the shap, one hot encoding and drop intercept
  X_all <- model.matrix(fml, data = test)[, -1]
  X_all <- as.data.frame(X_all)
  
  #functions that turns predictions into form fastshap package can work with
  pred_fun <- function(object, newdata) {
    predict(object, data = newdata)$predictions
  }
  
  #100 rows should give a good global image of feature importance
  shap_rows <- sample(nrow(X_all), min(100, nrow(X_all)))  #select random 100 rows
  shap_matrix <- fastshap::explain(
    object = model$finalModel,
    X = X_all[shap_rows, ],
    pred_wrapper = pred_fun,
    nsim = 10, #estimate marginal contributions of each feature in each prediction
    adjust = TRUE 
  )
  
  #save shaps
  shap_df <- as.data.frame(shap_matrix) 
  shap_df$obs_id <- shap_rows
  shap_df$target <- target
  
  shap_rf_results[[target]] <- shap_df
  
  print(Sys.time() - start_time) #to track loop time pt2
}

#flattens different shaps into one df
all_shaps_rf <- do.call(rbind, shap_rf_results)

#collapse dummies, otherwise we get an unreadable output
group_vars <- c("player_pos", "player_nat", "team_name", "counter_team_country", "country")
for (prefix in group_vars) {
  matching_cols <- grep(paste0("^", prefix), names(all_shaps_rf), value = TRUE)
  if (length(matching_cols) > 0) {
    all_shaps_rf[[prefix]] <- rowSums(all_shaps_rf[, matching_cols], na.rm = TRUE) #one shap per dummy var
    all_shaps_rf <- all_shaps_rf %>% select(-all_of(matching_cols))
  }
}

#calculate mean absolute SHAP for each feature per target-->how much does feature influence predictions
shap_summary_rf <- all_shaps_rf %>%
  select(-obs_id) %>%
  pivot_longer(-target, names_to = "feature", values_to = "shap_value") %>%
  group_by(target, feature) %>%
  summarise(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE), .groups = "drop")

