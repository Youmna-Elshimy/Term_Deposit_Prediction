#Group assignment 
setwd("~/Downloads/411tuning")
#importing libraries
library(corrplot)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(caret)       # For general data preparation and modeling
library(randomForest) # For the random forest algorithm, needed for RFE
library(ROSE)        # For dealing with imbalanced data
library(glmnet)
library(ROCR)

#import datasets
bank_full <- read.csv("bank-additional-full.csv", sep = ";")
bank_add <- read.csv("bank-additional.csv", sep = ";")


predictors <- c("duration", "euribor3m", "nr.employed", "pdays", "month")
response <- "y"

# Filtering the dataset to only use specified predictors and response

####THIS PART IS FOR TESTING BANK_ADD####
bank_data <- bank_add[, c(predictors, response)]
sapply(bank_data, function(x) sum(is.na(x)))

#checking structure of data
#RESULT = almost half are char and the other half is numeric
str(bank_full)
#str(bank_add)

#checking missing values (invalid data)
#RESULT = no missing values
sapply(bank_full, function(x) sum(is.na(x)))
sapply(bank_add, function(x) sum(is.na(x)))

#check for outliers
# Select only numeric columns for outlier analysis
numerical_cols <- sapply(bank_full, is.numeric) # finds all numeric columns
numeric_df <- bank_full[, numerical_cols] # subsets just these columns

# Add the outcome 'y' to this numeric dataframe for easier plotting
numeric_df$y <- bank_full$y

# Reshape the data into long format for faceting in ggplot2
numeric_df_long <- pivot_longer(numeric_df, 
                                cols = names(numeric_df)[names(numeric_df) != "y"], 
                                names_to = "variable",
                                values_to = "value")

# Generate boxplots for all numerical columns split by the 'y' outcome
ggplot(numeric_df_long, aes(x = y, y = value, fill = y)) + 
  geom_boxplot() + 
  facet_wrap(~variable, scales = "free_y") + 
  labs(title = "Original Boxplots of Numerical Variables by Outcome", x = "Outcome", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank()) +
  scale_fill_manual(values = c("no" = "lightblue", "yes" = "orange"))

# Convert the target variable 'y' to numeric (yes=1, no=0)
bank_full$y_numeric <- as.numeric(bank_full$y == "yes")

# Select only numeric variables, including the newly created y_numeric
numeric_data <- bank_full[,sapply(bank_full, is.numeric)]

# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")  # handling any possible missing values

# Plot correlation matrix with default visualization
corrplot(cor_matrix, main = "\nCorrelation Plot for Numerical Variables")

# Plot correlation matrix with correlation coefficients
corrplot(cor_matrix, main = "\nCorrelation Plot for Numerical Variables", method = "number")

# Copy and transform data
df_transformed <- bank_full
df_transformed$duration <- log1p(df_transformed$duration)
df_transformed$campaign <- pmin(df_transformed$campaign, quantile(df_transformed$campaign, 0.95))
df_transformed$age <- log1p(df_transformed$age)
df_transformed_data <- bank_data
df_transformed_data$duration <- log1p(df_transformed_data$duration)

# Select only numeric columns for transformed data visualization
numerical_cols_transformed <- sapply(df_transformed, is.numeric)
numeric_df_transformed <- df_transformed[, numerical_cols_transformed]
numeric_df_transformed$y <- df_transformed$y

# Reshape transformed data into long format for faceting in ggplot2
numeric_df_long_transformed <- pivot_longer(numeric_df_transformed, 
                                            cols = names(numeric_df_transformed)[names(numeric_df_transformed) != "y"], 
                                            names_to = "variable", 
                                            values_to = "value")

# Generate boxplots for transformed data
ggplot(numeric_df_long_transformed, aes(x = y, y = value, fill = y)) + 
  geom_boxplot() + 
  facet_wrap(~variable, scales = "free_y") + 
  labs(title = "Transformed Boxplots of Numerical Variables by Outcome", 
       x = "Outcome", 
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank()) +
  scale_fill_manual(values = c("no" = "lightblue", "yes" = "orange"))

# Convert the target variable 'y' to numeric (yes=1, no=0)
df_transformed$y_numeric <- as.numeric(df_transformed$y == "yes")

# Select only numeric variables, including the newly created y_numeric
numeric_data <- df_transformed[,sapply(df_transformed, is.numeric)]

# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")  # handling any possible missing values

# Plot correlation matrix with default visualization
corrplot(cor_matrix, main = "\nCorrelation Plot for Transformed Numerical Variables")

# Plot correlation matrix with correlation coefficients
corrplot(cor_matrix, main = "\nCorrelation Plot for Transformed Numerical Variables", method = "number")



# Convert appropriate columns to factors. Assuming from domain knowledge which ones are categorical
categorical_columns <- c('job', 'marital', 'education', 'default', 'housing', 'loan', 'contact', 
                         'month', 'day_of_week', 'poutcome', 'y')

df_transformed[categorical_columns] <- lapply(df_transformed[categorical_columns], as.factor)
bank_add[categorical_columns] <- lapply(bank_add[categorical_columns], as.factor)

str(df_transformed)

# Generating summary tables for categorical variables
cat_summary <- lapply(categorical_columns, function(x) {
  table(df_transformed[[x]], df_transformed$y)
})

# Printing the summary tables
print(cat_summary)

# Summary statistics for numerical variables
numerical_vars <- sapply(df_transformed, is.numeric)
num_summary <- summary(df_transformed[, numerical_vars])
print(num_summary)

#plotting the relationships between each attribute and the target variable y
# Visualizing categorical variables with respect to y
cat_vs_y <- c('marital', 'job', 'education', 'default', 'housing',
              'loan', 'contact', 'month', 'day_of_week', 'poutcome')

cat_plots_y <- lapply(cat_vs_y, function(x) {
  ggplot(df_transformed, aes_string(x = x, fill = 'y')) +
    geom_bar(position = "fill") + coord_flip() +
    labs(title = paste("Proportion of", x, "by subscription"), 
         x = x, y = "Proportion") +
    theme_minimal()
})

grid.arrange(grobs = cat_plots_y, ncol = 3)


#plotting the relationships between attributes
# Visualizing categorical data with respect to age
cat_vs_col <- c('marital', 'job', 'education', 'default', 'housing',
                'loan', 'contact', 'month', 'day_of_week', 'poutcome')

num_plots_1 <- lapply(cat_vs_col, function(x) {
  ggplot(df_transformed, aes(x = x, y = age)) +
    geom_boxplot() +
    labs(title = paste(x, "VS age"))
})

grid.arrange(grobs = num_plots_1, ncol = 3)

# Visualizing categorical data with respect to duration
num_plots_2 <- lapply(cat_vs_col, function(x) {
  ggplot(df_transformed, aes(x = x, y = duration)) +
    geom_boxplot() +
    labs(title = paste(x, "VS last call's duration"))
})

grid.arrange(grobs = num_plots_2, ncol = 3)

# Visualizing categorical data with respect to campaign
num_plots_3 <- lapply(cat_vs_col, function(x) {
  ggplot(df_transformed, aes(x = x, y = campaign)) +
    geom_boxplot() +
    labs(title = paste(x, "VS campaign"))
})

grid.arrange(grobs = num_plots_3, ncol = 3)

# Visualizing categorical data with respect to pdays
num_plots_4 <- lapply(cat_vs_col, function(x) {
  ggplot(df_transformed, aes(x = x, y = pdays)) +
    geom_boxplot() +
    labs(title = paste(x, "VS pdays"))
})

grid.arrange(grobs = num_plots_4, ncol = 3)

# Visualizing categorical data with respect to previous
num_plots_5 <- lapply(cat_vs_col, function(x) {
  ggplot(df_transformed, aes(x = x, y = previous)) +
    geom_boxplot() +
    labs(title = paste(x, "VS previous calls"))
})

grid.arrange(grobs = num_plots_5, ncol = 3)

# Visualizing categorical data with respect to emp.var.rate
num_plots_6 <- lapply(cat_vs_col, function(x) {
  ggplot(df_transformed, aes(x = x, y = emp.var.rate)) +
    geom_boxplot() +
    labs(title = paste(x, "VS employment variation"))
})

grid.arrange(grobs = num_plots_6, ncol = 3)

# Visualizing categorical data with respect to cons.price.idx
num_plots_7 <- lapply(cat_vs_col, function(x) {
  ggplot(df_transformed, aes(x = x, y = cons.price.idx)) +
    geom_boxplot() +
    labs(title = paste(x, "VS consumer price index"))
})

grid.arrange(grobs = num_plots_7, ncol = 3)

# Visualizing categorical data with respect to cons.conf.idx
num_plots_8 <- lapply(cat_vs_col, function(x) {
  ggplot(df_transformed, aes(x = x, y = cons.conf.idx)) +
    geom_boxplot() +
    labs(title = paste(x, "VS consumer confidence index"))
})

grid.arrange(grobs = num_plots_8, ncol = 3)

# Visualizing categorical data with respect to euribor3m
num_plots_9 <- lapply(cat_vs_col, function(x) {
  ggplot(df_transformed, aes(x = x, y = euribor3m)) +
    geom_boxplot() +
    labs(title = paste(x, "VS euribor 3 month rate"))
})

grid.arrange(grobs = num_plots_9, ncol = 3)

# Visualizing categorical data with respect to nr.employed
num_plots_10 <- lapply(cat_vs_col, function(x) {
  ggplot(df_transformed, aes(x = x, y = nr.employed)) +
    geom_boxplot() +
    labs(title = paste(x, "VS number of employees"))
})

grid.arrange(grobs = num_plots_10, ncol = 3)

# Visualizing categorical variables with respect to marital
marital_vs_col <- c('job', 'education', 'default', 'housing',
                    'loan', 'contact', 'month', 'day_of_week', 'poutcome')

cat_plots_1 <- lapply(marital_vs_col, function(x) {
  ggplot(df_transformed, aes_string(x = x, fill = 'marital')) +
    geom_bar(position = "fill") + coord_flip() +
    labs(title = paste("Proportion of", x, "by marital status"), 
         x = x, y = "Proportion") +
    theme_minimal()
})

grid.arrange(grobs = cat_plots_1, ncol = 3)


# Visualizing categorical variables with respect to education
education_vs_col <- c('job', 'month')

cat_plots_2 <- lapply(education_vs_col, function(x) {
  ggplot(df_transformed, aes_string(x = x, fill = 'education')) +
    geom_bar(position = "fill") + coord_flip() +
    labs(title = paste("Proportion of", x, "by education level"), 
         x = x, y = "Proportion") +
    theme_minimal()
})

grid.arrange(grobs = cat_plots_2, ncol = 2)


# Visualizing categorical variables with respect to default
default_vs_col <- c('job', 'education', 'housing',
                    'loan', 'contact', 'month', 'day_of_week', 'poutcome')

cat_plots_3 <- lapply(default_vs_col, function(x) {
  ggplot(df_transformed, aes_string(x = x, fill = 'default')) +
    geom_bar(position = "fill") + coord_flip() +
    labs(title = paste("Proportion of", x, "by credit in default"), 
         x = x, y = "Proportion") +
    theme_minimal()
})

grid.arrange(grobs = cat_plots_3, ncol = 3)


# Visualizing categorical variables with respect to housing
housing_vs_col <- c('job', 'education', 'loan', 'contact', 
                    'month', 'day_of_week', 'poutcome')

cat_plots_4 <- lapply(housing_vs_col, function(x) {
  ggplot(df_transformed, aes_string(x = x, fill = 'housing')) +
    geom_bar(position = "fill") + coord_flip() +
    labs(title = paste("Proportion of", x, "by credit in default"), 
         x = x, y = "Proportion") +
    theme_minimal()
})

grid.arrange(grobs = cat_plots_4, ncol = 3)


# Visualizing categorical variables with respect to loan
loan_vs_col <- c('job', 'contact', 'month', 'day_of_week', 'poutcome', 'education')

cat_plots_5 <- lapply(loan_vs_col, function(x) {
  ggplot(df_transformed, aes_string(x = x, fill = 'loan')) +
    geom_bar(position = "fill") + coord_flip() +
    labs(title = paste("Proportion of", x, "by personal loan"), 
         x = x, y = "Proportion") +
    theme_minimal()
})

grid.arrange(grobs = cat_plots_5, ncol = 3)

# Visualizing categorical variables with respect to contact
contact_vs_col <- c('job', 'month', 'day_of_week', 'poutcome', 'education')

cat_plots_6 <- lapply(contact_vs_col, function(x) {
  ggplot(df_transformed, aes_string(x = x, fill = 'contact')) +
    geom_bar(position = "fill") + coord_flip() +
    labs(title = paste("Proportion of", x, "by contact type"),
         x = x, y = "Proportion") +
    theme_minimal()
})

grid.arrange(grobs = cat_plots_6, ncol = 3)

# Visualizing categorical variables with respect to month
month_vs_col <- c('job', 'day_of_week', 'poutcome')

cat_plots_7 <- lapply(month_vs_col, function(x) {
  ggplot(df_transformed, aes_string(x = x, fill = 'month')) +
    geom_bar(position = "fill") + coord_flip() +
    labs(title = paste("Proportion of", x, "by last contact month"), 
         x = x, y = "Proportion") +
    theme_minimal()
})

grid.arrange(grobs = cat_plots_7, ncol = 2)

# Visualizing categorical variables with respect to day of the week
day_vs_col <- c('job', 'poutcome', 'education')

cat_plots_8 <- lapply(day_vs_col, function(x) {
  ggplot(df_transformed, aes_string(x = x, fill = 'day_of_week')) +
    geom_bar(position = "fill") + coord_flip() +
    labs(title = paste("Proportion of", x, "by last contact week day"), 
         x = x, y = "Proportion") +
    theme_minimal()
})

grid.arrange(grobs = cat_plots_8, ncol = 2)

# Visualizing categorical variables with respect to poutcome
poutcome_vs_col <- c('job', 'education')

cat_plots_9 <- lapply(poutcome_vs_col, function(x) {
  ggplot(df_transformed, aes_string(x = x, fill = 'poutcome')) +
    geom_bar(position = "fill") +
    labs(title = paste("Proportion of", x, "by last campaign's outcome"), 
         x = x, y = "Proportion") +
    theme_minimal()
})

grid.arrange(grobs = cat_plots_9, ncol = 1)



# Attribute selection
# Set seed for reproducibility
set.seed(123)

# Create a data partition for training and testing sets
index <- createDataPartition(df_transformed$y, p = 0.8, list = FALSE)
train_data <- df_transformed[index, ]
test_data <- df_transformed[-index, ]

# Convert 'y' into factor if it's binary categorical
train_data$y <- as.factor(train_data$y)
test_data$y <- as.factor(test_data$y)
bank_data$y <- as.factor(df_transformed_data$y)
str(bank_data)

# Define the predictors and response variable:
#predictors <- train_data[, -which(names(train_data) %in% c("y", "y_numeric"))]
#response <- train_data$y

# Run RFE:
#rfe_results <- rfe(x = predictors, y = response, sizes = c(1:5), rfeControl = control)

# Print the results:
#print(rfe_results)

# Plot the RFE results:
#plot(rfe_results, type = c("g", "o"))

# Result :The top 5 variables(out of 20):duration, euribor3m, nr.employed, pdays, month



# Data pre-processing
# Define the predictor variables and response variable
predictors <- c("duration", "euribor3m", "nr.employed", "pdays", "month")
response <- "y"

# Filter the dataset only using specified predictors and response
train_data <- train_data[, c(predictors, response)]
test_data <- test_data[, c(predictors, response)]
bank_data <- bank_data[, c(predictors, response)]

# One-hot encoding for 'month' if it's categorical
# Convert 'month' into factor 
train_data$month <- as.factor(train_data$month)
test_data$month <- as.factor(test_data$month)
bank_data$month <- as.factor(bank_data$month)

# Create dummy variables only for 'month', directly add to data frames
train_dummies <- model.matrix(~ month - 1, data = train_data)
test_dummies <- model.matrix(~ month - 1, data = test_data)
bank_dummies <- model.matrix(~ month - 1, data = bank_data)

# Bind the dummy variables back to the main data frame
train_data_transformed <- cbind(train_data[, setdiff(predictors, "month")], train_dummies, y = train_data$y)
test_data_transformed <- cbind(test_data[, setdiff(predictors, "month")], test_dummies, y = test_data$y)
bank_data_transformed <- cbind(bank_data[, setdiff(predictors, "month")], bank_dummies, y = bank_data$y)

# Scaling numeric features (excluding 'month' dummies and 'y')
numeric_features <- c("duration", "euribor3m", "nr.employed", "pdays") # Only numeric predictors
preProcValues <- preProcess(train_data_transformed[, numeric_features], method = c("center", "scale"))

# Apply scaling to numeric features only
train_data_scaled <- predict(preProcValues, train_data_transformed[, numeric_features])
test_data_scaled <- predict(preProcValues, test_data_transformed[, numeric_features])
bank_data_scaled <- predict(preProcValues, bank_data_transformed[, numeric_features])

# Combine scaled numeric features with one-hot encoded 'month' and 'y'
train_data_scaled <- data.frame(train_data_scaled, train_dummies, y = train_data$y)
test_data_scaled <- data.frame(test_data_scaled, test_dummies, y = test_data$y)
bank_data_scaled <- data.frame(bank_data_scaled, bank_dummies, y = bank_data$y)

# Print the structure to confirm setup
print(str(train_data_scaled))
print(str(test_data_scaled))
print(str(bank_data_scaled))



# Logistic Regression Model
set.seed(123)  # for reproducibility

# Define training control
train_control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

# Train the model using the entire dataset (excluding the need to manually exclude columns)
logistic_model <- train(
  y ~ ., 
  data = train_data_scaled,  # already properly formatted dataset
  method = "glm",
  family = binomial,
  trControl = train_control
)
print(logistic_model)
# Output the model summary
summary(logistic_model$finalModel)

# Visualize the result

# Extract coefficients and standard errors
coefficients <- as.numeric(summary(logistic_model$finalModel)$coefficients[, "Estimate"])
std_errors <- as.numeric(summary(logistic_model$finalModel)$coefficients[, "Std. Error"])

# Get attribute names
attribute_names <- rownames(summary(logistic_model$finalModel)$coefficients)

# Create a data frame for plotting
coef_df <- data.frame(
  attribute = attribute_names,
  coefficient = coefficients,
  std_error = std_errors
)

# Plot coefficients with error bars
p <- ggplot(coef_df, aes(x = attribute, y = coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.6) +
  geom_errorbar(aes(ymin = coefficient - std_error, ymax = coefficient + std_error), 
                width = 0.4, color = "black", position = position_dodge(width = 0.9)) +
  labs(title = "Coefficient Estimates with Standard Errors",
       x = "Attributes", y = "Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

###These significant predictors selection make the model become overfitted (Do not use)### 
###############################################################
# Define the subset of significant predictors based on previous model outputs or domain knowledge
significant_predictors <- c("duration", "euribor3m", "nr.employed", "pdays",
                            "monthaug", "monthjul", "monthjun", "monthmar", 
                            "monthmay", "monthoct")

# Select significant predictor variables and the response variable from the scaled dataset
train_data_significant <- train_data_scaled[, c(significant_predictors, "y")]
test_data_significant <- test_data_scaled[, c(significant_predictors, "y")]
bank_data_significant <- bank_data_scaled[, c(significant_predictors, "y")]

# Train the logistic regression model using only significant features
logistic_model_significant <- train(
  y ~ ., 
  data = train_data_scaled,
  method = "glm",
  family = binomial,
  trControl = train_control
)
# Output the significant model summary to evaluate the model
summary(logistic_model_significant$finalModel)
##################################################################
# Predict on the test data using the predictors model
predictions_significant <- predict(logistic_model_significant, newdata = cbind(test_data_scaled, y = test_data_scaled$y))

# Convert actual responses into factor with same levels as predictions
actual_responses_significant <- as.factor(test_data_significant$y)  # Convert to factor

# Calculate and print the accuracy of the model with significant predictors
conf_matrix_logistic <- confusionMatrix(predictions_significant, actual_responses_significant)

# Display the confusion matrix for logistic regression model of test set
print(conf_matrix_logistic)

# Make the confusion matrix plot
confusionMatrixPlot <- ggplot(data = as.data.frame(conf_matrix_logistic$table), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Frequency", x = "Actual Value", y = "Predicted Value", title = "Confusion Matrix for Logistic Regression Model")

# Print the plot
print(confusionMatrixPlot)



# Random Forest Model

# Perform oversampling to balance the dataset
desired_N <- max(2 * sum(train_data_scaled$y == "no"), nrow(train_data_scaled))
train_data_balanced <- ovun.sample(y ~ ., data = train_data_scaled, method = "over", 
                                   N = desired_N, seed = 123)$data

# Check the distribution of 'y' after balancing
table(train_data_balanced$y)

# Train Random Forest Model with balanced data
set.seed(123)  # For reproducibility
random_forest_model <- randomForest(y ~ ., data = train_data_balanced, ntree = 100)

# Check to make sure the model is set for classification
print(random_forest_model)

# Predict and evaluate model performance on the test data
predictions_rf <- predict(random_forest_model, newdata = test_data_scaled)

# Use the same factor object to ensure consistency in levels
conf_matrix_rf <- confusionMatrix(predictions_rf, actual_responses_significant)
print(conf_matrix_rf)

# Print variable importance
print(varImp(random_forest_model))



# Obtain variable importance
var_importance <- as.data.frame(varImp(random_forest_model))
var_importance$Variable <- rownames(var_importance)

# Plot variable importance
ggplot(var_importance, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  xlab("Variable") +
  ylab("Importance") +
  ggtitle("Variable Importance Plot") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert the confusion matrix to long format for plotting using ggplot2 
conf_matrix_long <- as.data.frame(as.table(conf_matrix_rf$table))

# Plot the confusion matrix
ggplot(conf_matrix_long, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("Actual Value") +
  ylab("Predicted Value") +
  ggtitle("Confusion Matrix") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))



#Tuning Hyperparameters
#Tuning Logistic regression model

train_control_tuned <- trainControl(
  method ="cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE
)

set.seed(123)
#Tuning grid
tuning.grid <- expand.grid(
  alpha = 0.5,
  lambda = 0.001
)
#tuned logistic regression model
logistic_model_tuned <- train(
  y ~ ., 
  data = train_data_scaled,  # already properly formatted dataset
  method = "glmnet",
  trControl = train_control_tuned,
  tuneGrid = tuning.grid,
  preProcess = "scale"
)
print(logistic_model_tuned)
print(logistic_model)


#Tuned Logistic regression Model

# Predict on the test data using the predictors model
predictions_tuned <- predict(logistic_model_tuned, newdata = cbind(test_data_scaled, y = test_data_scaled$y))

# Convert actual responses to factor with same levels as predictions
actual_responses_significant <- as.factor(test_data_scaled$y)  # Convert to factor

# Calculate and print the accuracy of the model with significant predictors
conf_matrix_logistic <- confusionMatrix(predictions_tuned, actual_responses_significant)
print(conf_matrix_logistic)

#Hyperparameter tuning for randomForest model

####RANDOM FOREST MODEL WITH TUNING####

#Best tuning model for RF
random_forest_model <- randomForest(y ~ ., data = train_data_balanced,mtry=15, ntree =1000,nodesize=5)
print(random_forest_model)  
pred <- predict(random_forest_model, newdata = test_data_scaled) 

conf_matrix_rf <- confusionMatrix(pred, actual_responses_significant)
print(conf_matrix_rf)

###################### Before tuning vs After tuning ROC for Logistic Regression #########################

# Predict on the test data using the significant predictors model
predictions_significant_withouttune <- predict(logistic_model, newdata = cbind(test_data_scaled, y = test_data_scaled$y))
predictions_tuned_tuned <- predict(logistic_model_tuned, newdata = cbind(test_data_scaled, y = test_data_scaled$y))
# Assuming predictions_significant_withouttune is a factor variable with levels "yes" and "no"
prediction_counts <- table(test_data_scaled$y)
print(prediction_counts)

print(logistic_model_tuned)
print(logistic_model)
# Convert actual responses to factor with same levels as predictions
actual_responses_significant <- as.factor(test_data_scaled$y)  # Convert to factor

pred_logistic_withouttune <- prediction(as.numeric(predictions_significant), as.numeric(actual_responses_significant))
pred_logistic_tuned <- prediction(as.numeric(predictions_tuned_tuned), as.numeric(actual_responses_significant))

perf_logistic_withouttune <- performance(pred_logistic_withouttune,"tpr","fpr")
perf_logistic_tuned <- performance(pred_logistic_tuned,"tpr","fpr")

auc_logistic_withouttune <- performance(pred_logistic_withouttune,measure ="auc")
auc_logistic_tuned <- performance(pred_logistic_tuned,measure ="auc")

print(paste("AUC Logistic Regression:",auc_logistic_withouttune@y.values[[1]]))
print(paste("AUC Logistic Regression After Tuned:",auc_logistic_tuned@y.values[[1]]))

plot(perf_logistic_withouttune,col="green",main="ROC Curves Comparision")
plot(perf_logistic_tuned,col="red",add=TRUE)
legend("bottomright",legend=c("Without Tune","Tuned"),col=c("green","red"),lwd=2)

###################### Before tuning vs After tuning ROC for Random Forest #########################


pred_rf <- prediction(as.numeric(predictions_rf), as.numeric(actual_responses_significant))
pred_rf_tuned <- prediction(as.numeric(pred),as.numeric(actual_responses_significant))

perf_rf <- performance(pred_rf,"tpr","fpr")
perf_rf_tuned <- performance(pred_rf_tuned,"tpr","fpr")

auc_rf <- performance(pred_rf,measure ="auc")
auc_rf_tuned <- performance(pred_rf_tuned,measure ="auc")

print(paste("AUC Random Forest:",auc_rf@y.values[[1]]))
print(paste("AUC Random Forest Tuned:",auc_rf_tuned@y.values[[1]]))

plot(perf_rf,col="green",main="ROC Curves Comparision")
plot(perf_rf_tuned,col="red",add=TRUE)
legend("bottomright",legend=c("Random Forest","Random Forest Tuned"),col=c("green","red"),lwd=2)

###################### Before tuning vs After tuning ROC for Random Forest #########################

predictions_tuned_tuned <- predict(logistic_model_tuned, newdata = cbind(test_data_scaled, y = test_data_scaled$y))
actual_responses_significant <- as.factor(test_data_scaled$y)  # Convert to factor

e
pred_logistic_tuned <- prediction(as.numeric(predictions_tuned_tuned), as.numeric(actual_responses_significant))
pred_rf_tuned <- prediction(as.numeric(pred),as.numeric(actual_responses_significant))

perf_logistic_tuned <- performance(pred_logistic_tuned,"tpr","fpr")
perf_rf_tuned <- performance(pred_rf_tuned,"tpr","fpr")

auc_logistic <- performance(pred_logistic_tuned,measure ="auc")
auc_rf <- performance(pred_rf_tuned,measure ="auc")

print(paste("AUC Logistic Regression Tuned:",auc_logistic@y.values[[1]]))
print(paste("AUC Random Forest TUned:",auc_rf@y.values[[1]]))

plot(perf_logistic,col="green",main="ROC Curves Comparision")
plot(perf_rf,col="red",add=TRUE)
legend("bottomright",legend=c("Logistic Regression Tuned","Random Forest Tuned"),col=c("green","red"),lwd=2)
