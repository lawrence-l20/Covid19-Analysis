library(ggplot2)

county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_2/COVID_19_National_Subset_CSV.csv")

# show summary statistics for the Deaths_to_Pop_Percentage column
summary(county_data$Deaths_to_Pop_Percentage)

ggplot(county_data, aes(x = "", y = Deaths_to_Pop_Percentage)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = 0.1, fill = "white", outlier.color = "red") +
  labs(x = "", y = "Deaths to Population Percentage") +
  ggtitle("Distribution of Deaths to Population Percentage")



# define the Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# find the mode of the Deaths_to_Pop_Percentage column
mode_value <- Mode(county_data$Deaths_to_Pop_Percentage)

print(mode_value)










# calculate the standard deviation of the Deaths_to_Pop_Percentage column
sd_value <- sd(county_data$Deaths_to_Pop_Percentage)

# print the standard deviation value
print(sd_value)



#Performing LASSO

library(glmnet)


# Drop all rows with missing values
county_data <- na.omit(county_data)

# Extract the attributes and target variable
X <- as.matrix(county_data[, c("Population_1_year_and_over", "Confirmed", "Deaths", "geo_id", "median_year_structure_built", "Percent_income_spent_on_rent", "Percent_Male", "median_income", "Percent_graduate_or_more", "aggregate_time_travel_work", "genie_index")])
y <- as.numeric(county_data$Deaths_to_Pop_Percentage)

# Split the data into training and testing sets
set.seed(123)
train <- sample(nrow(X), nrow(X) * 0.7)
X_train <- X[train, ]
X_test <- X[-train, ]
y_train <- y[train]
y_test <- y[-train]
# Perform LASSO regression using glmnet
lasso_model <- glmnet(X_train, y_train, alpha = 1)

# Find the optimal lambda value using cross-validation
cv_model <- cv.glmnet(X_train, y_train, alpha = 1)
lambda_opt <- cv_model$lambda.min

# Use the optimal lambda value to train the final model
lasso_model_opt <- glmnet(X_train, y_train, alpha = 1, lambda = lambda_opt)

# Extract the coefficients of the final model and sort by magnitude
coef_df <- data.frame(variable = colnames(X), coef = coef(lasso_model_opt)[-1])
coef_df <- coef_df[order(abs(coef_df$coef), decreasing = TRUE), ]

# Print the sorted coefficients
print(coef_df)


print(county_data)






library(glmnet)
library(car)

# Drop all rows with missing values
county_data <- na.omit(county_data)

# Extract the attributes and target variable
X <- as.matrix(county_data[, c("Population_1_year_and_over", "Confirmed", "Deaths", "geo_id", "median_year_structure_built", "Percent_income_spent_on_rent", "Percent_Male", "median_income", "Percent_graduate_or_more", "aggregate_time_travel_work", "genie_index")])
y <- as.numeric(county_data$Deaths_to_Pop_Percentage)

# Combine the predictor variables and response variable into a data frame
data <- data.frame(X, Deaths_to_Pop_Percentage = y)

# Calculate the VIF scores for the predictor variables
vif_scores <- vif(lm(Deaths_to_Pop_Percentage ~ ., data = data))

# Print the VIF scores
print(vif_scores)

# Create a bar chart of the VIF scores
barplot(vif_scores, col = "blue", main = "VIF Scores for Predictor Variables", xlab = "Predictor Variable", ylab = "VIF Score")










#Finding Cook's Statistic:

install.packages("tidyverse")

library(tidyverse)
library(broom)


# Split your data into predictors (X) and target variable (y)
# Split your data into predictors (X) and target variable (y)
X <- county_data %>% select(-Deaths_to_Pop_Percentage)
y <- county_data$Deaths_to_Pop_Percentage

# Fit a logistic regression model
log_reg <- glm(y ~ ., data = X, family = "binomial")

# Calculate Cook's distance for each data point
cooks_dist <- augment(log_reg) %>% mutate(cooks_dist = .cooksd)

# Set a threshold for influential outliers
cooks_threshold <- 4 / (nrow(X) - ncol(X) - 1)

# Identify influential outliers
outliers <- cooks_dist %>% filter(cooks_dist > cooks_threshold)

# Print the influential outliers
print(outliers)



avg_population <- mean(outliers$Population_1_year_and_over, na.rm = TRUE)
cat("Average Population 1 year and over in outliers:", avg_population)

print(mean(outliers$median_income))
print(mean(county_data$median_income))


print(mean(outliers$median_year_structure_built))
print(mean(county_data$median_year_structure_built))



#Determine the amount of missing data in each column. 

# Read in the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_2/COVID_19_National_Subset_CSV.csv")

# Count the number of NA values in each column
na_counts <- colSums(is.na(county_data))

# Print the results
print(na_counts)






#Determining the means of tuning hyperparameters (Random Forest)

library(randomForest)

# Define the hyperparameter grid
param_grid <- expand.grid(
  mtry = c(2, 3, 4, 5),
  ntree = c(500, 1000, 2000),
  nodesize = c(1, 5, 10)
)

# Print the number of hyperparameter combinations
cat("Number of hyperparameter combinations:", nrow(param_grid), "\n")




#Determining method of tuning hyperparameters (SVM):
library(e1071)
param_grid <- expand.grid(
  cost = 10^(-1:2),
  gamma = c(0.1, 1, 10),
  kernel = c("linear", "radial")
)
cat("Number of hyperparameter combinations:", nrow(param_grid), "\n")







#Determining the means of tuning hyperparameters (Logistic Regression)
library(glmnet)

#Define the hyperparameter grid
param_grid <- expand.grid(
  alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  lambda = c(0.001, 0.01, 0.1, 1, 10, 100),
  family = c("binomial")
)

#Print the number of hyperparameter combinations
cat("Number of hyperparameter combinations:", nrow(param_grid), "\n")




#Random Forest Hyperparameter Tuning

# install.packages("mlr")

ptm <- proc.time()
library(ranger)
library(mlr)

# Load your dataset
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_2/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert 'County_Name' column to a factor
county_data$County_Name <- as.factor(county_data$County_Name)
county_data$State <- as.factor(county_data$State)


# Define task and learner
task <- makeRegrTask(id = "county_data",
                     data = county_data,
                     target = "Deaths_to_Pop_Percentage")

learner <- makeLearner("regr.ranger")

# Choose resampling strategy and define grid
rdesc <- makeResampleDesc("CV", iters = 5)
ps <- makeParamSet(makeIntegerParam("mtry", lower = 2, upper = 5),
                   makeDiscreteParam("num.trees", 200))

# Tune
res = tuneParams(learner, task, rdesc, par.set = ps,
                 control = makeTuneControlGrid())

# Train on entire dataset (using best hyperparameters)
lrn = setHyperPars(makeLearner("regr.ranger"), par.vals = res$x)
m = train(lrn, task)

print(m)
print(proc.time() - ptm)

cat("Best hyperparameters:\n")
print(res$x)






#SVM Hyperparameter Tuning:


ptm <- proc.time()
library(kernlab)
library(mlr)

# Load your dataset
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_2/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert 'County_Name' column to a factor
county_data$County_Name <- as.factor(county_data$County_Name)
county_data$State <- as.factor(county_data$State)

# Define task and learner
task <- makeRegrTask(id = "county_data",
                     data = county_data,
                     target = "Deaths_to_Pop_Percentage")

learner <- makeLearner("regr.ksvm")

# Choose resampling strategy and define grid
rdesc <- makeResampleDesc("CV", iters = 5)
ps <- makeParamSet(makeNumericParam("C", lower = 0.1, upper = 10),
                   makeNumericParam("sigma", lower = 0.1, upper = 1))

# Tune
res = tuneParams(learner, task, rdesc, par.set = ps,
                 control = makeTuneControlGrid())

# Train on entire dataset (using best hyperparameters)
lrn = setHyperPars(makeLearner("regr.ksvm"), par.vals = res$x)
m = train(lrn, task)

print(m)
print(proc.time() - ptm)

cat("Best hyperparameters:\n")
print(res$x)






# Load required libraries
library(mlr)

# Load the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_2/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert 'County_Name' and 'State' columns to factors
county_data$County_Name <- as.factor(county_data$County_Name)
county_data$State <- as.factor(county_data$State)
county_data$Deaths_to_Pop_Percentage <- as.factor(county_data$Deaths_to_Pop_Percentage)


# Define task and learner
task <- makeClassifTask(data = county_data, target = "Deaths_to_Pop_Percentage")
learner <- makeLearner("classif.rpart")

# Define parameter grid
param_grid <- makeParamSet(
  makeNumericParam("cp", lower = 0.0001, upper = 0.01)
)

# Set up the resampling strategy
rdesc <- makeResampleDesc("CV", iters = 5)

# Tune the model using random search
set.seed(123)
tune_result <- tuneParams(
  learner = learner,
  task = task,
  resampling = rdesc,
  par.set = param_grid,
  control = makeTuneControlRandom(maxit = 20)
)

# Train the model on the full dataset using the optimal hyperparameters
lasso_lr <- setHyperPars(learner, par.vals = tune_result$x)
lasso_lr_model <- train(lasso_lr, task)

# Print the optimal hyperparameters
print(tune_result$x)






#Random Forest Classification:

# Load the required libraries
library(randomForest)
library(caret)

# Load the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_3/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert the target variable to factor
county_data$Deaths_to_Pop_Percentage_Classifier <- as.factor(county_data$Deaths_to_Pop_Percentage_Classifier)

# Set up the cross-validation
folds <- createFolds(county_data$Deaths_to_Pop_Percentage_Classifier, k = 5)

# Define the hyperparameter grid
param_grid <- expand.grid(
  mtry = c(2, 3, 4, 5)
)

# Use grid search to find optimal hyperparameters
rf_grid <- train(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data, 
  method = "rf", 
  trControl = trainControl(method = "cv", index = folds, verboseIter=10), 
  tuneGrid = param_grid
)

# Print the optimal hyperparameters
print(rf_grid$bestTune)

# Train the final model on the full dataset using the optimal hyperparameters
rf_final <- randomForest(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data,
  mtry = rf_grid$bestTune$mtry,
  ntree = 200,
  nodesize = rf_grid$bestTune$nodesize
)

# Print the model accuracy
print(paste("Accuracy:", mean(rf_final$predicted == county_data$Deaths_to_Pop_Percentage_Classifier)))

# Generate predictions for the full dataset
predictions <- predict(rf_final, county_data)

# Find the observations where the prediction is "Extreme"
extreme_counties <- county_data$County_Name[predictions == "High"]

# Print the list of counties
print(extreme_counties)


#random Forest w/ Confusion Matrix:

#Random Forest Classification:

# Load the required libraries
library(randomForest)
library(caret)

# Load the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_3/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert the target variable to factor
county_data$Deaths_to_Pop_Percentage_Classifier <- as.factor(county_data$Deaths_to_Pop_Percentage_Classifier)

# Set up the cross-validation
folds <- createFolds(county_data$Deaths_to_Pop_Percentage_Classifier, k = 5)

# Define the hyperparameter grid
param_grid <- expand.grid(
  mtry = c(2, 3, 4, 5)
)

# Use grid search to find optimal hyperparameters
rf_grid <- train(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data, 
  method = "rf", 
  trControl = trainControl(method = "cv", index = folds, verboseIter=10), 
  tuneGrid = param_grid
)

# Print the optimal hyperparameters
print(rf_grid$bestTune)

# Train the final model on the full dataset using the optimal hyperparameters
rf_final <- randomForest(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data,
  mtry = rf_grid$bestTune$mtry,
  ntree = 200,
  nodesize = rf_grid$bestTune$nodesize
)

# Print the model accuracy and confusion matrix
predicted <- predict(rf_final, county_data)
conf_mat <- confusionMatrix(predicted, county_data$Deaths_to_Pop_Percentage_Classifier)
print(paste("Accuracy:", conf_mat$overall["Accuracy"]))
print(conf_mat$table)







#More stats with the Random Forest:


#Random Forest Classification:

# Load the required libraries
library(randomForest)
library(caret)

# Load the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_3/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert the target variable to factor
county_data$Deaths_to_Pop_Percentage_Classifier <- as.factor(county_data$Deaths_to_Pop_Percentage_Classifier)

# Set up the cross-validation
folds <- createFolds(county_data$Deaths_to_Pop_Percentage_Classifier, k = 5)

# Define the hyperparameter grid
param_grid <- expand.grid(
  mtry = c(2, 3, 4, 5)
)

# Use grid search to find optimal hyperparameters
rf_grid <- train(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data, 
  method = "rf", 
  trControl = trainControl(method = "cv", index = folds, verboseIter=10), 
  tuneGrid = param_grid
)

# Print the optimal hyperparameters
print(rf_grid$bestTune)

# Train the final model on the full dataset using the optimal hyperparameters
rf_final <- randomForest(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data,
  mtry = rf_grid$bestTune$mtry,
  ntree = 200
)

# Print the model accuracy
print(paste("Accuracy:", mean(rf_final$predicted == county_data$Deaths_to_Pop_Percentage_Classifier)))

# Calculate the confusion matrix
cm <- confusionMatrix(rf_final$predicted, county_data$Deaths_to_Pop_Percentage_Classifier)

print(cm)
# Calculate and print the F1 score
f1_score <- 2 * (cm$byClass['Sensitivity'] * cm$byClass['Specificity']) / (cm$byClass['Sensitivity'] + cm$byClass['Specificity'])
print(paste("F1 Score:", f1_score))








#Random Forest Classification:

#Random Forest Classification:

# Load the required libraries
library(randomForest)
library(caret)

# Load the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_3/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert the target variable to factor
county_data$Deaths_to_Pop_Percentage_Classifier <- as.factor(county_data$Deaths_to_Pop_Percentage_Classifier)

# Set up the cross-validation
folds <- createFolds(county_data$Deaths_to_Pop_Percentage_Classifier, k = 5)

# Define the hyperparameter grid
param_grid <- expand.grid(
  mtry = c(2, 3, 4, 5)
)

# Use grid search to find optimal hyperparameters
rf_grid <- train(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data, 
  method = "rf", 
  trControl = trainControl(method = "cv", index = folds, verboseIter=10), 
  tuneGrid = param_grid
)

# Print the optimal hyperparameters
print(rf_grid$bestTune)

# Train the final model on the full dataset using the optimal hyperparameters
rf_final <- randomForest(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data,
  mtry = rf_grid$bestTune$mtry,
  ntree = 200
)

# Print the model accuracy
print(paste("Accuracy:", mean(rf_final$predicted == county_data$Deaths_to_Pop_Percentage_Classifier)))

# Check for NA values in predictions and target variable
print(paste("NA values in predictions:", sum(is.na(rf_final$predicted))))
print(paste("NA values in target variable:", sum(is.na(county_data$Deaths_to_Pop_Percentage_Classifier))))

# Calculate the confusion matrix
cm <- table(rf_final$predicted, county_data$Deaths_to_Pop_Percentage_Classifier)

# Manually calculate precision, recall, and F1 score
true_positives <- diag(cm)
false_positives <- colSums(cm) - true_positives
false_negatives <- rowSums(cm) - true_positives

precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)

f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the precision, recall, and F1 score for each class  2=medium, 3=high, 1=low, 4=extreme:
for (i in seq_along(f1_score)) {
  cat(paste("Class", i, "Precision:", precision[i], "\n"))
  cat(paste("Class", i, "Recall:", recall[i], "\n"))
  cat(paste("Class", i, "F1 Score:", f1_score[i], "\n\n"))
}

print(cm)


# Display unique values and their respective counts
unique_classes <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
cat("Unique Classes:", unique_classes, "\n")

value_counts <- table(county_data$Deaths_to_Pop_Percentage_Classifier)
print(value_counts)


# Create a named vector to map class numbers to actual values
class_mapping <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
names(class_mapping) <- paste("Class", 1:length(class_mapping))

# Print the class mapping
print(class_mapping)

# Example: Get the actual value for Class 2
class_1_value <- class_mapping["Class 1"]
print(paste("Class 3 corresponds to:", class_1_value))




#plotting random forest:

# Load the required libraries
library(randomForest)
library(caret)
library(ggplot2)

# Load the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_3/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert the target variable to factor
county_data$Deaths_to_Pop_Percentage_Classifier <- as.factor(county_data$Deaths_to_Pop_Percentage_Classifier)

# Set up the cross-validation
folds <- createFolds(county_data$Deaths_to_Pop_Percentage_Classifier, k = 5)

# Define the hyperparameter grid
param_grid <- expand.grid(
  mtry = c(2, 3, 4, 5)
)

# Use grid search to find optimal hyperparameters
rf_grid <- train(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data, 
  method = "rf", 
  trControl = trainControl(method = "cv", index = folds, verboseIter=10), 
  tuneGrid = param_grid
)

# Print the optimal hyperparameters
print(rf_grid$bestTune)

# Train the final model on the full dataset using the optimal hyperparameters
rf_final <- randomForest(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data,
  mtry = rf_grid$bestTune$mtry,
  ntree = 200
)

# Print the model accuracy
print(paste("Accuracy:", mean(rf_final$predicted == county_data$Deaths_to_Pop_Percentage_Classifier)))
# Create a data frame with the predicted and actual values, including Deaths_to_Pop_Percentage
results <- data.frame(predicted = rf_final$predicted,
                      actual = county_data$Deaths_to_Pop_Percentage_Classifier,
                      Deaths_to_Pop_Percentage = county_data$Deaths_to_Pop_Percentage)

# Add a column indicating whether the prediction was correct or not
results$is_correct <- results$predicted == results$actual

# Create a scatter plot with blue dots for correct predictions and red dots for incorrect predictions
ggplot(results, aes(x = Deaths_to_Pop_Percentage, y = State, color = is_correct)) +
  geom_point() +
  labs(title = "Random Forest Classification Results",
       subtitle = paste("Accuracy:", round(mean(rf_final$predicted == county_data$Deaths_to_Pop_Percentage_Classifier), 3)),
       x = "Deaths_to_Pop_Percentage",
       y = "State",
       color = "Correct Classification") +
  scale_color_manual(values = c("red", "blue"))








library(ggplot2)

# Create a data frame with the predicted and actual values
results <- data.frame(predicted = results$predicted,
                      actual = results$actual,
                      Deaths_to_Pop_Percentage = results$Deaths_to_Pop_Percentage,
                      is_correct = results$is_correct)

# Create a scatter plot with blue dots for correct predictions and red dots for incorrect predictions
ggplot(results, aes(x = Deaths_to_Pop_Percentage, y = actual, color = is_correct)) +
  geom_point() +
  labs(title = "Random Forest Classification Results",
       subtitle = paste("Accuracy:", round(mean(results$is_correct), 3)),
       x = "Deaths_to_Pop_Percentage",
       y = "Actual Classification",
       color = "Correct Classification") +
  scale_color_manual(values = c("red", "blue"))









#SVM Algorithm:

# Support Vector Machines Classification

# Load the required libraries
library(e1071)
library(caret)

# Load the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_3/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert the target variable to factor
county_data$Deaths_to_Pop_Percentage_Classifier <- as.factor(county_data$Deaths_to_Pop_Percentage_Classifier)

# Set up the cross-validation
folds <- createFolds(county_data$Deaths_to_Pop_Percentage_Classifier, k = 5)

# Define the hyperparameter grid
# Define the hyperparameter grid
param_grid <- expand.grid(
  C = 2^(seq(-5, 15, by = 2)),
  sigma = 2^(seq(-15, 3, by = 2))
)

# Use grid search to find optimal hyperparameters
svm_grid <- train(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data, 
  method = "svmRadial", 
  trControl = trainControl(method = "cv", index = folds, verboseIter=10), 
  tuneGrid = param_grid
)


# Print the optimal hyperparameters
print(svm_grid$bestTune)

# Train the final model on the full dataset using the optimal hyperparameters
# Train the final model on the full dataset using the optimal hyperparameters
svm_final <- svm(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data,
  cost = 0.03125,
  sigma = 8
)


# Make predictions
predictions <- predict(svm_final, county_data)

# Create a data frame with the predicted and actual classifications
results <- data.frame(
  predicted = predictions,
  actual = county_data$Deaths_to_Pop_Percentage_Classifier,
  is_correct = (predictions == county_data$Deaths_to_Pop_Percentage_Classifier)
)



# Print the model accuracy
print(paste("Accuracy:", mean(predictions == county_data$Deaths_to_Pop_Percentage_Classifier)))

# Calculate the confusion matrix
cm <- table(predictions, county_data$Deaths_to_Pop_Percentage_Classifier)

# Manually calculate precision, recall, and F1 score
true_positives <- diag(cm)
false_positives <- colSums(cm) - true_positives
false_negatives <- rowSums(cm) - true_positives

precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)

f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the precision, recall, and F1 score for each class 2=medium, 3=high, 1=low, 4=extreme:
for (i in seq_along(f1_score)) {
  cat(paste("Class", i, "Precision:", precision[i], "\n"))
  cat(paste("Class", i, "Recall:", recall[i], "\n"))
  cat(paste("Class", i, "F1 Score:", f1_score[i], "\n\n"))
}

print(cm)

# Display unique values and their respective counts
unique_classes <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
cat("Unique Classes:", unique_classes, "\n")

value_counts <- table(county_data$Deaths_to_Pop_Percentage_Classifier)
print(value_counts)

# Create a named vector to map class numbers to actual values
class_mapping <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
names(class_mapping) <- paste("Class", 1:length(class_mapping))

# Print the class mapping
print(class_mapping)

# Example: Get the actual value for Class 2
class_1_value



# Create a new data frame with the Deaths_to_Pop_Percentage and predicted classification
svm_results <- data.frame(
  Deaths_to_Pop_Percentage = county_data$Deaths_to_Pop_Percentage,
  predicted = predictions
)

# Create a scatter plot with blue dots for correct predictions and red dots for incorrect predictions
ggplot(svm_results, aes(x = Deaths_to_Pop_Percentage, y = predicted, color = predictions == county_data$Deaths_to_Pop_Percentage_Classifier)) +
  geom_point() +
  labs(title = "SVM Classification Results",
       subtitle = paste("Accuracy:", round(mean(predictions == county_data$Deaths_to_Pop_Percentage_Classifier), 3)),
       x = "Deaths_to_Pop_Percentage",
       y = "Predicted Classification",
       color = "Correct Classification") +
  scale_color_manual(values = c("red", "blue"))










#SVM Algorithm With Graph Model:
#SVM Algorithm:

# Support Vector Machines Classification

# Load the required libraries
library(e1071)
library(caret)
library(ggplot2)

# Load the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_3/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert the target variable to factor
county_data$Deaths_to_Pop_Percentage_Classifier <- as.factor(county_data$Deaths_to_Pop_Percentage_Classifier)

# Set up the cross-validation
folds <- createFolds(county_data$Deaths_to_Pop_Percentage_Classifier, k = 5)

# Define the hyperparameter grid
param_grid <- expand.grid(
  C = 2^(seq(-5, 15, by = 2)),
  sigma = 2^(seq(-15, 3, by = 2))
)

# Use grid search to find optimal hyperparameters
svm_grid <- train(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data, 
  method = "svmRadial", 
  trControl = trainControl(method = "cv", index = folds, verboseIter=10), 
  tuneGrid = param_grid
)


# Print the optimal hyperparameters
print(svm_grid$bestTune)

# Train the final model on the full dataset using the optimal hyperparameters
svm_final <- svm(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data,
  cost = 5.6,
  sigma = 0.1
)


# Make predictions
predictions <- predict(svm_final, county_data)

# Create a data frame with the predicted and actual classifications
results <- data.frame(
  predicted = predictions,
  actual = county_data$Deaths_to_Pop_Percentage_Classifier,
  is_correct = (predictions == county_data$Deaths_to_Pop_Percentage_Classifier)
)



# Print the model accuracy
print(paste("Accuracy:", mean(predictions == county_data$Deaths_to_Pop_Percentage_Classifier)))

# Calculate the confusion matrix
cm <- table(predictions, county_data$Deaths_to_Pop_Percentage_Classifier)

# Manually calculate precision, recall, and F1 score
true_positives <- diag(cm)
false_positives <- colSums(cm) - true_positives
false_negatives <- rowSums(cm) - true_positives

precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)

f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the precision, recall, and F1 score for each class 2=medium, 3=high, 1=low, 4=extreme:
for (i in seq_along(f1_score)) {
  cat(paste("Class", i, "Precision:", precision[i], "\n"))
  cat(paste("Class", i, "Recall:", recall[i], "\n"))
  cat(paste("Class", i, "F1 Score:", f1_score[i], "\n\n"))
}

print(cm)

# Create a named vector to map class numbers to actual values
class_mapping <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
names(class_mapping) <- paste("Class", 1:length(class_mapping))

# Print the class mapping
print(class_mapping)

# Display unique values and their respective counts
unique_classes <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
cat("Unique Classes:", unique_classes, "\n")

value_counts <- table(county_data$Deaths_to_Pop_Percentage_Classifier)
print(value_counts)

  




# Display unique values and their respective counts
unique_classes <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
cat("Unique Classes:", unique_classes, "\n")

value_counts <- table(county_data$Deaths_to_Pop_Percentage_Classifier)
print(value_counts)

# Create a named vector to map class numbers to actual values
class_mapping <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
names(class_mapping) <- paste("Class", 1:length(class_mapping))

# Print the class mapping
print(class_mapping)

# Example: Get the actual value for Class 2
class_1_value <- class_mapping["Class 1"]
print(paste("Class 3 corresponds to:", class_1_value))

# ROC curve calculation and plotting for each class
for (class in unique_classes) {
  # Convert target variable to binary (One-vs-Rest approach)
  binary_targets <- ifelse(county_data$Deaths_to_Pop_Percentage_Classifier == class, 1, 0)
  
  # Calculate probabilities for the current class
  class_probs <- rf_final$votes[, class]
  
  # Calculate ROC curve
  roc_obj <- roc(binary_targets, class_probs)
  
  # Plot ROC curve
  plot(roc_obj, main = paste("ROC Curve for Class", class))
}































#logistic Regression:

#Logistic Regression Algorithm:
 # Load the required libraries
library(caret)

#Load the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_3/COVID_19_National_Subset_CSV.csv")

#Remove rows with missing values
county_data <- na.omit(county_data)

#Convert the target variable to factor
county_data$Deaths_to_Pop_Percentage_Classifier <- as.factor(county_data$Deaths_to_Pop_Percentage_Classifier)

#Set up the cross-validation
folds <- createFolds(county_data$Deaths_to_Pop_Percentage_Classifier, k = 5)
# Define the hyperparameter grid
param_grid <- expand.grid(
  alpha = c(0, 0.25, 0.5, 0.75, 1), lambda = 2^(seq(-5, 5, by = 2)),
  penalty = c("l1", "l2")
)

# Use grid search to find optimal hyperparameters
logit_grid <- train(
  Deaths_to_Pop_Percentage_Classifier ~ .,
  data = county_data,
  method = "glmnet",
  family = "multinomial",
  trControl = trainControl(method = "cv", index = folds, verboseIter=10),
  tuneGrid = param_grid,
)





#Print the optimal hyperparameters
print(logit_grid$bestTune)

#Train the final model on the full dataset using the optimal hyperparameters
logit_final <- glmnet(
  Deaths_to_Pop_Percentage_Classifier ~ .,
  data = county_data,
  family = "multinomial",
  alpha = 1, # Lasso
  lambda = logit_grid$bestTune$cost,
  penalty.factor = ifelse(logit_grid$bestTune$penalty == "l1", 1, 0))

#Make predictions
predictions <- predict(logit_final, newx = county_data, type = "class")

#Create a data frame with the predicted and actual classifications
results <- data.frame(
  predicted = predictions,
  actual = county_data$Deaths_to_Pop_Percentage_Classifier,
  is_correct = (predictions == county_data$Deaths_to_Pop_Percentage_Classifier)
)

#Print the model accuracy
print(paste("Accuracy:", mean(predictions == county_data$Deaths_to_Pop_Percentage_Classifier)))

#Calculate the confusion matrix
cm <- table(predictions, county_data$Deaths_to_Pop_Percentage_Classifier)

#Manually calculate precision, recall, and F1 score
true_positives <- diag(cm)
false_positives <- colSums(cm) - true_positives
false_negatives <- rowSums(cm) - true_positives

precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)

f1_score <- 2 * (precision * recall) / (precision + recall)

#Print the precision, recall, and F1 score for each class 2=medium, 3=high, 1=low, 4=extreme:
  for (i in seq_along(f1_score)) {
    cat(paste("Class", i, "Precision:", precision[i], "\n"))
    cat(paste("Class", i, "Recall:", recall[i], "\n"))
    cat(paste("Class", i, "F1 Score:", f1_score[i], "\n\n"))
  }

print(cm)

#Display unique values and their respective counts
unique_classes <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
cat("Unique Classes:", unique_classes, "\n")

#value_counts <- table(county_data$Deaths_to_Pop_Percentage_Classifier






# Logistic Regression Modeling:
library(tidyverse)
library(caret)
library(glmnet)

data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_3/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
data <- na.omit(data)

# Split the dataset (75% train, 25% test)
set.seed(123)
split_index <- createDataPartition(data$Deaths_to_Pop_Percentage_Classifier, p = 0.75, list = FALSE)
train_data <- data[split_index, ]
test_data <- data[-split_index, ]

# One-hot encode categorical variables
train_dummies <- dummyVars(~ ., data = data) # Use the entire dataset to create the encoding rules
train_encoded <- predict(train_dummies, newdata = train_data)
train_encoded <- as.data.frame(train_encoded)

# Create the model matrix
x_train <- model.matrix(Deaths_to_Pop_Percentage_Classifier ~ . - 1, data = train_data)
x_test <- model.matrix(Deaths_to_Pop_Percentage_Classifier ~ . - 1, data = test_data)

# Use cross-validation to find the optimal lambda value
cv_glmnet <- cv.glmnet(x_train, y_train, family = "multinomial", type.measure = "class", nfolds = 5, alpha = 1)

# Train the logistic regression model with optimal lambda value
model <- glmnet(x_train, y_train, family = "multinomial", lambda = cv_glmnet$lambda.min, alpha = 1)

# Create the model matrix for the test set
x_test <- model.matrix(Deaths_to_Pop_Percentage_Classifier ~ . - 1, data = test_data)
y_test <- test_data$Deaths_to_Pop_Percentage_Classifier

#Determine the larger dimension for each axis
max_rows <- max(dim(x_train)[1], dim(x_test)[1])
max_cols <- max(dim(x_train)[2], dim(x_test)[2])

# Resize x_train and x_test using a loop
for (i in 1:2) {
  if (dim(x_train)[i] < max_rows || dim(x_test)[i] < max_rows) {
    if (i == 1) {
      new_rows <- max_rows - dim(x_train)[1]
      x_train <- rbind(x_train, matrix(0, nrow = new_rows, ncol = dim(x_train)[2]))
      new_rows <- max_rows - dim(x_test)[1]
      x_test <- rbind(x_test, matrix(0, nrow = new_rows, ncol = dim(x_test)[2]))
    } else {
      new_cols <- max_cols - dim(x_train)[2]
      x_train <- cbind(x_train, matrix(0, nrow = dim(x_train)[1], ncol = new_cols))
      new_cols <- max_cols - dim(x_test)[2]
      x_test <- cbind(x_test, matrix(0, nrow = dim(x_test)[1], ncol = new_cols))
    }
  }
}

# Make predictions on the test set
predictions <- predict(model, newx = x_test, type = "class", s = cv_glmnet$lambda.min)

# Compute the confusion matrix, F1 Score, recall, and precision
conf_matrix <- confusionMatrix(predictions, y_test)
print(conf_matrix)


# Check dimensions of train and test sets
cat("Training set dimensions:\n")
print(dim(predictions))

cat("Test set dimensions:\n")
print(dim(y_test))















# Multinomial Logistic Regression:

# Load the required libraries
library(caret)
library(nnet)

# Load the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_3/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert the target variable to factor
county_data$Deaths_to_Pop_Percentage_Classifier <- as.factor(county_data$Deaths_to_Pop_Percentage_Classifier)

# Set up the cross-validation
folds <- createFolds(county_data$Deaths_to_Pop_Percentage_Classifier, k = 5)

# Train multinomial logistic regression model with cross-validation
# Train multinomial logistic regression model with cross-validation
# Train multinomial logistic regression model with cross-validation
# Train the final model on the full dataset with an increased MaxNWts
multinom_final <- multinom(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data,
  MaxNWts = 10000 # Increase the maximum allowed weights
)


# Calculate the predicted class labels
predicted_labels <- predict(multinom_final, county_data)

# Print the model accuracy
print(paste("Accuracy:", mean(predicted_labels == county_data$Deaths_to_Pop_Percentage_Classifier)))

# Calculate the confusion matrix
cm <- table(predicted_labels, county_data$Deaths_to_Pop_Percentage_Classifier)

# Manually calculate precision, recall, and F1 score
true_positives <- diag(cm)
false_positives <- colSums(cm) - true_positives
false_negatives <- rowSums(cm) - true_positives

precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)

f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the precision, recall, and F1 score for each class
for (i in seq_along(f1_score)) {
  cat(paste("Class", i, "Precision:", precision[i], "\n"))
  cat(paste("Class", i, "Recall:", recall[i], "\n"))
  cat(paste("Class", i, "F1 Score:", f1_score[i], "\n\n"))
}

print(cm)

# Display unique values and their respective counts
unique_classes <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
cat("Unique Classes:", unique_classes, "\n")

value_counts <- table(county_data$Deaths_to_Pop_Percentage_Classifier)
print(value_counts)

# Create a named vector to map class numbers to actual values
class_mapping <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
names(class_mapping) <- paste("Class", 1:length(class_mapping))

# Print the class mapping
print(class_mapping)

# Example: Get the actual value for Class 2
class_1_value <- class_mapping["Class 3"]
print(paste("Class 3 corresponds to:", class_1_value))



# Print the coefficients associated with each attribute for each class
coefs <- coef(multinom_final)
for (i in seq_along(unique_classes)) {
  cat(paste("\nCoefficients for Class", i, ":\n"))
  print(coefs[, i])
}
# Print the coefficients associated with each attribute for each class
coefs <- coef(multinom_final)

# Get the model formula
model_formula <- as.formula(multinom_final$call$formula)

# Extract the predictor variable names from the formula
predictor_vars <- as.character(model_formula[-(1:2)])

# Set the rownames of the coefs matrix to match the predictor variables in the model
rownames(coefs) <- predictor_vars

# Set the colnames of the coefs matrix to match the class names
colnames(coefs) <- paste0("Class_", 1:(length(unique_classes) - 1))

# Print the coefs matrix
cat("\nCoefficients:\n")
print(coefs)







#Multinomial regression PCA Dimensionality Reduction

# Load the required library
library(ggplot2)

# Remove non-numeric columns from the data
numeric_data <- county_data[, sapply(county_data, is.numeric)]

# Perform PCA on the predictor variables
pca <- prcomp(numeric_data[, !colnames(numeric_data) %in% "Deaths_to_Pop_Percentage_Classifier"], scale = TRUE)

# Transform the original data using the first two principal components
pca_data <- as.data.frame(predict(pca, numeric_data)[, 1:2])
colnames(pca_data) <- c("PC1", "PC2")

# Add the target variable to the PCA data
pca_data$Deaths_to_Pop_Percentage_Classifier <- county_data$Deaths_to_Pop_Percentage_Classifier

# Fit the multinomial logistic regression model on the PCA data
multinom_pca <- multinom(
  Deaths_to_Pop_Percentage_Classifier ~ PC1 + PC2,
  data = pca_data,
  MaxNWts = 10000
)

# Define a function to create a grid of points and make predictions using the model
predict_multinom <- function(model, grid) {
  grid$pred <- predict(model, grid, type = "class")
  return(grid)
}

# Create a grid of points covering the PCA space
grid <- expand.grid(
  PC1 = seq(min(pca_data$PC1), max(pca_data$PC1), length.out = 200),
  PC2 = seq(min(pca_data$PC2), max(pca_data$PC2), length.out = 200)
)

# Make predictions on the grid using the PCA model
grid_preds <- predict_multinom(multinom_pca, grid)

# Plot the decision boundaries and the data points
ggplot(grid_preds, aes(x = PC1, y = PC2, color = pred)) +
  geom_tile() +
  geom_point(data = pca_data, aes(color = Deaths_to_Pop_Percentage_Classifier, shape = Deaths_to_Pop_Percentage_Classifier), size = 3, alpha = 0.8) +
  scale_color_discrete("Classifier") +
  scale_shape_discrete("Classifier") +
  labs(title = "Decision Boundary Scatterplot", x = "PC1", y = "PC2") +
  theme_minimal()










# Multinomial Logistic Regression:

# Load the required libraries
library(caret)
library(nnet)
library(stats4) # Add this package for logLik function

# Load the data
county_data <- read.csv("C:/Users/Lawrence_Lim/Box/Spring_2023/Data_Mining/Project_3/COVID_19_National_Subset_CSV.csv")

# Remove rows with missing values
county_data <- na.omit(county_data)

# Convert the target variable to factor
county_data$Deaths_to_Pop_Percentage_Classifier <- as.factor(county_data$Deaths_to_Pop_Percentage_Classifier)

# Set up the cross-validation
folds <- createFolds(county_data$Deaths_to_Pop_Percentage_Classifier, k = 5)

# Train multinomial logistic regression model with cross-validation
# Train multinomial logistic regression model with cross-validation
# Train multinomial logistic regression model with cross-validation
# Train the final model on the full dataset with an increased MaxNWts
multinom_final <- multinom(
  Deaths_to_Pop_Percentage_Classifier ~ ., 
  data = county_data,
  MaxNWts = 10000 # Increase the maximum allowed weights
)

# Calculate the predicted class labels
predicted_labels <- predict(multinom_final, county_data)

# Print the model accuracy
print(paste("Accuracy:", mean(predicted_labels == county_data$Deaths_to_Pop_Percentage_Classifier)))

# Calculate the confusion matrix
cm <- table(predicted_labels, county_data$Deaths_to_Pop_Percentage_Classifier)

# Manually calculate precision, recall, and F1 score
true_positives <- diag(cm)
false_positives <- colSums(cm) - true_positives
false_negatives <- rowSums(cm) - true_positives

precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)

f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the precision, recall, and F1 score for each class
for (i in seq_along(f1_score)) {
  cat(paste("Class", i, "Precision:", precision[i], "\n"))
  cat(paste("Class", i, "Recall:", recall[i], "\n"))
  cat(paste("Class", i, "F1 Score:", f1_score[i], "\n\n"))
}

print(cm)

# Display unique values and their respective counts
unique_classes <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
cat("Unique Classes:", unique_classes, "\n")

value_counts <- table(county_data$Deaths_to_Pop_Percentage_Classifier)
print(value_counts)

# Create a named vector to map class numbers to actual values
class_mapping <- unique(county_data$Deaths_to_Pop_Percentage_Classifier)
names(class_mapping) <- paste("Class", 1:length(class_mapping))

# Print the class mapping
print(class_mapping)

# Example: Get the actual value for Class 2
class_1_value <- class_mapping["Class 3"]
print(paste("Class 3 corresponds to:", class_1_value))

# Print the coefficients associated with each attribute for each class
coefs <- coef(multinom_final)
for (i in seq_along(unique_classes)) {
  cat(paste("\nCoefficients for Class", i, ":\n"))
  print(coefs)}
        

