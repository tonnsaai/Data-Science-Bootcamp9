library(tidyverse)
library(mlbench)
library(caret)
library(MLmetrics)

#Machine Learning model to predict churn in mobile data usage

churn_data <- read_csv("churn.csv")
mean(complete.cases(churn_data)) #to check Null in Dataset

#Steps
## 1. split data
split_churn <- function(churn_data, size=0.8) {
                    set.seed(86)
                    n <- nrow(churn_data)
                    train_churn <- sample(1:n, size*n)
                    train_df <- churn_data[train_churn, ]
                    test_df <- churn_data[-train_churn, ]
                    return( list(train_df, test_df) )
                  }
churn_df <- split_churn(churn_data, size=0.8)

## 2. train model
ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = prSummary,
                     classProbs = TRUE)

churn_model <- train(churn ~ .,
                     data = churn_df[[1]],
                     method = "knn",
                     metric = "Precision",
                     trControl=ctrl
                    )

## 3. score model
predict_churn <- predict(churn_model, newdata = churn_df[[2]])

## 4. evaluate model
actual_churn <- factor(churn_df[[2]]$churn)

mean(actual_churn == pred_churn)

confusionMatrix(data = predict_churn, 
                reference = actual_churn,
                positive="Yes",
                mode="prec_recall")
