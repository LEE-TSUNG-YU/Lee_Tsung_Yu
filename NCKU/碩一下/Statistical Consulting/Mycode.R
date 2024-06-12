# Introduction ####

## Data source ####
# The laptop dataset is from Kaggle [Laptop Sales Price Prediction](https://www.kaggle.com/datasets/siddiquifaiznaeem/laptop-sales-price-prediction-dataset-2024).
laptop <- read.csv("C:/github_LTY/homework/Final Report/data/laptop.csv",
                   na.strings = c("","NA"))
## Summary statistics ####
# #| output: asis
# latex(describe(laptop), file = "", caption.placement = "top")

## Missing Values ####
plot_missing(laptop, missing_only = TRUE)

## EDA ####
### Fix wrong values ####

# Brand
laptop[laptop$Brand == "Asus", "Brand"] = "ASUS" # Fix "ASUS"
# index <- which(sort(table(laptop$Brand)) < 10)
# names <- rownames(sort(table(laptop$Brand)))[index]
# for(i in length(names)){
#   laptop[laptop$Brand == names[i], "Brand"] = "Other"
# }

# Price
laptop$Price <- laptop$Price*0.39 # From INR to TWD.(1 : 0.39)

# Processor_brand
# index <- which(sort(table(laptop$Processor_brand)) < 10)
# names <- rownames(sort(table(laptop$Processor_brand)))[index]
# for(i in length(names)){
#   laptop[laptop$Processor_brand == names[i], "Processor_brand"] = "Other"
# }

# Processor_name
sort(table(laptop$Processor_name))
laptop[laptop$Brand == "Intel Core 3", "Processor_name"] = "Intel Core i3"
laptop[laptop$Brand == "Intel Core 5", "Processor_name"] = "Intel Core i5"
laptop[laptop$Brand == "Intel Core 7", "Processor_name"] = "Intel Core i7"

# RAM_type
laptop[867, "RAM_type"] <- "LPDDR5X"
laptop$RAM_type <- gsub("LPDDRX4", "LPDDR4X", laptop$RAM_type)


# Data Analysis ####
## Feature Engineering ####

## Split into training and testing (8:2) ####
data <- read.csv("C:/github_LTY/homework/Final Report/data/laptop_encoded.csv",
                   na.strings = c("","NA"))
library(splitTools)
smp.size = floor(0.8*nrow(data))
train.index = sample(seq_len(nrow(data)), smp.size)
train = data[train.index, ] # 80%
test = data[-train.index, ] # 20%

## Model Fitting ####

MAE <- function(pred, true){
  return(mean(abs(pred-true)))
}
RMSE <- function(pred, true){
  return(sqrt(mean((pred-true)^2)))
}
### Linear Regression ####
model_lm <- lm(Price_TWD ~ ., data = train)
summary(model_lm)
pred_lm <- predict(model_lm, test[, -c(13)], type = "response")
lm_MAE <- MAE(pred_lm, test$Price_TWD)
lm_RMSE <- RMSE(pred_lm, test$Price_TWD)
cat("Linear MAE:  ", MAE(pred_lm, test$Price_TWD),
    "Linear RMSE: ", RMSE(pred_lm, test$Price_TWD))

### SVM ####
library(e1071)
model_svm <- svm(Price_TWD ~ ., type = "eps-regression", kernel = "linear", data = train)
pred_svm <- predict(model_svm, test[, -c(13)], type = "response")
svm_MAE <- MAE(pred_svm, test$Price_TWD)
svm_RMSE <- RMSE(pred_svm, test$Price_TWD)
cat("SVM MAE:  ", MAE(pred_svm, test$Price_TWD),
    "SVM RMSE: ", RMSE(pred_svm, test$Price_TWD))

### Random Forest ####
library(randomForest)
model_rf <- randomForest(Price_TWD ~ ., importance = T, data = train)
pred_rf <- predict(model_rf, test[, -c(13)], type = "response")
rf_MAE <- MAE(pred_rf, test$Price_TWD)
rf_RMSE <- RMSE(pred_rf, test$Price_TWD)
cat("RF MAE:  ", MAE(pred_rf, test$Price_TWD),
    "RF RMSE: ", RMSE(pred_rf, test$Price_TWD))

### XGBoost ####
library(xgboost)

model_xgb <- xgboost(data = X, label = Y, nrounds = 10000, objective = "reg:absoluteerror", verbose = 0)
pred_xgb <- predict(model_xgb, test[, -c(13)], type = "response")
xgb_MAE <- MAE(pred_xgb, test$Price_TWD)
xgb_RMSE <- RMSE(pred_xgb, test$Price_TWD)
cat("XGB MAE:  ", MAE(pred_xgb, test$Price_TWD),
    "XGB RMSE: ", RMSE(pred_xgb, test$Price_TWD))

### LightGBM ####
library(lightgbm)
Y <- train$Price_TWD
X <- as.matrix(train[, -c(13)])
model_gbm <- lightgbm(data = X, label = Y, verbose = 2,
                      params = list(objective = "regression", metric = "rmse"))
predict(model_gbm, test, type = "response")

### Catboost ####
library(catboost)
library(catboost)
Y <- rnorm(100, 5, 2) + rnorm(100, 50, 1)
X <- as.matrix(data.frame(X1 <- rnorm(100,5,2),
                          X2 <- rnorm(100,50,1)))
train_pool <- catboost.load_pool(data = train, label = Y)
fit_params <- list(loss_function = 'RMSE',
                   iterations = 100)
# train model
model_cat <- catboost.train(train_pool, params = fit_params)
# test data
test_pool <- catboost.load_pool(data = test, label = Y)
# predict
prediction <- catboost.predict(model_cat, test_pool)

# Final Results ####

