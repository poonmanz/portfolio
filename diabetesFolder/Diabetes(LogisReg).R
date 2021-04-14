# Logistics regression
# predict diabetes
## By Peson Suksamai

# load dataset
diabetes <- read.csv("R/diabetes.csv")

# preview dataset
head(diabetes)
tail(diabetes)

# review data type in our dataframe
str(diabetes)

diabetes$diabetes <- as.factor(diabetes$diabetes)

# count frequency
table(diabetes$diabetes) / nrow(diabetes)

# split dataset
set.seed(42)
n <- nrow(diabetes)
train_id <- sample(1:n, size = 0.8*n)
train_data <- diabetes[train_id, ]
test_data <- diabetes[-train_id, ]

# train model
logisticModel <- glm(diabetes ~ glucose + age + mass,
                     data = train_data,
                     family = "binomial")

# test model
p <- predict(logisticModel, newdata = test_data, type = "response")
predictions <- ifelse(p > 0.5, "pos", "neg")
predictions[1:20]

# prediction == actual
mean(predictions == test_data$diabetes)

# confusion matrix
table(predictions, test_data$diabetes, dnn = c("predicted", "actual"))











