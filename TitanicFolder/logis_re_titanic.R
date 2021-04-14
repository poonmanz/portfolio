## Logistic Regression (Titanic)
## By Peson Suksamai
library(titanic)

head(titanic_train)

## Drop NA (missing values)
titanic_train <- na.omit(titanic_train)
nrow(titanic_train)

## SPLIT DATA
set.seed(42)
n <- nrow(titanic_train)
id <- sample(1: n, size = n*0.7) ## 70% train 30% test
train_data <- titanic_train[id, ]
test_data <- titanic_train[-id, ]

## Train Model
model <- glm(Survived ~ Pclass + Age + Parch, data = train_data, family = "binomial")

summary(model)

## Predict and Evaluate Model (train_data)
train_data$prob_survived <- predict(model, type = "response")
train_data$pred_survived <- ifelse(train_data$prob_survived >= 0.5, 1, 0)

## confusion matrix (train_data)
confusion_matrix_train <- table(train_data$pred_survived, train_data$Survived, 
                          dnn = c("Predicted", "Actual"))

## Model Evaluation (train_data)
A_train <- (confusion_matrix_train[1,1] + confusion_matrix_train[2,2]) / sum(confusion_matrix_train)
P_train <- confusion_matrix_train[2,2] / (confusion_matrix_train[2,1] + confusion_matrix_train[2,2])
R_train <- confusion_matrix_train[2,2] / (confusion_matrix_train[1,2] + confusion_matrix_train[2,2])
F1_train <- 2 * (P_train * R_train)/(P_train + R_train)



## Test Model
## Predict and Evaluate Model (test_data)
test_data$prob_survived <- predict(model, newdata = test_data, type = "response")
test_data$pred_survived <- ifelse(test_data$prob_survived >= 0.5, 1, 0)

## confusion matrix (test_data)
confusion_matrix_test <- table(test_data$pred_survived, test_data$Survived, 
                          dnn = c("Predicted", "Actual"))

## Model Evaluation (test_data)
A_test <- (confusion_matrix_test[1,1] + confusion_matrix_test[2,2]) / sum(confusion_matrix_test)
P_test <- confusion_matrix_test[2,2] / (confusion_matrix_test[2,1] + confusion_matrix_test[2,2]) 
R_test <- confusion_matrix_test[2,2] / (confusion_matrix_test[1,2] + confusion_matrix_test[2,2])
F1_test <- 2 * ((P_test * R_test) / (P_test + R_test))

## Accuracy
cat("Accuracy Train Model:", A_train,
    "\nPrecsion Train Model:", P_train,
    "\nRecall Train Model:", R_train,
    "\nF1 Train Model:", F1_train,
    "\n=========================================",
    "\nAccuracy Test Model:", A_test,
    "\nPrecsion Test Model:", P_test,
    "\nRecall Test Model:", R_test,
    "\nF1 Test Model:", F1_test)
























