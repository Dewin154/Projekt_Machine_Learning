# filepath: /home/nico/dev/Projekt_Machine_Learning/decision_tree.r

# Load required libraries
library(rpart)
library(rpart.plot)

# Source the cleaning script to get the cleaned data
source("cleaning_script.R")

set.seed(42) # For reproducibility

# Set the target variable and predictors
target <- "price_in_euro"
predictors <- setdiff(names(data), target)

# Split data: 70% train, 15% val, 15% test
n <- nrow(data)
idx <- sample(seq_len(n))
train_idx <- idx[1:round(0.7 * n)]
val_idx <- idx[(round(0.7 * n) + 1):(round(0.85 * n))]
test_idx <- idx[(round(0.85 * n) + 1):n]

train_data <- data[train_idx, ]
val_data <- data[val_idx, ]
test_data <- data[test_idx, ]

# Create the formula for the decision tree
formula <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))

# Fit the decision tree model on training data
tree_model <- rpart(formula, data = train_data, method = "anova")

# Plot the decision tree
rpart.plot(tree_model, main = "Decision Tree for Car Price Prediction")

# Predict on validation and test sets
val_pred <- predict(tree_model, val_data)
test_pred <- predict(tree_model, test_data)

# Calculate RMSE function
rmse <- function(actual, predicted) {
    sqrt(mean((actual - predicted)^2))
}

# Print results
cat("Validation RMSE:", rmse(val_data[[target]], val_pred), "\n")
cat("Test RMSE:", rmse(test_data[[target]], test_pred), "\n")
