## Decission Tree (Iris)
## By Peson Suksamai
## library
library(rpart)
library(rpart.plot)
## load dataset
iris <- read.csv("R/iris.csv", stringsAsFactors = TRUE)

## preview dataset
head(iris)

## review structure
str(iris)

# split dataset
set.seed(42)
n <- nrow(iris)
train_id <- sample(1:n, size = 0.8*n)
train_data <- iris[train_id, ] ## 80%
test_data <- iris[-train_id, ] ## 20%

## train model Decision Tree
TreeModelIris <- rpart(species ~ .,
                       data = train_data,
                       method = "class")

rpart.plot(TreeModelIris)

## test model Decision Tree
p <- predict(TreeModelIris, newdata = test_data, type = "class")

## accuracy (Actual = Prediction)
mean(p == test_data$species)




















