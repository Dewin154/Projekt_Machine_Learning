library(caret)
library(doParallel)

cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)

#cc <- seq(-1, 2, 1)    # Weniger Werte für cost
#cg <- seq(-2, -1, 0.5) # Weniger Werte für gamma
cc <- seq(0, 2, 0.5)    # 0, 0.5, 1, 1.5, 2
cg <- seq(-2, -1, 0.5) # -2, -1.75, -1.5, -1.25, -1, ergibt 5 Werte


ctrl <- trainControl(method = "cv", number = 3, allowParallel = TRUE)
grid <- expand.grid(
  .C = 2^cc,
  .sigma = 1/(2 * 10^cg)
)

model <- train(
  price_in_euro ~ power_kw + transmission_type + mileage_in_km + sporty + age_in_months + fuel_type_new + brand_type,
  data = data.train_and_val,
  method = "svmRadial",
  tuneGrid = grid,
  trControl = ctrl
)

stopCluster(cl)

# Calculation of prediction results on the test data:
X.test <- data.test[, c("power_kw", "transmission_type", "mileage_in_km", "sporty", "age_in_months", "fuel_type_new", "brand_type")]
prognosen <- predict(model, X.test)
# Calculation of the mean absolute error (MAE)
y.test <- data.test[,"price_in_euro"]
mae <- mean(abs(y.test - prognosen))
cat("Mean Absolute Error (MAE):", mae, "\n")

# Print the best tuning parameters
cat("Best tuning parameters:\n")
print(model$bestTune)

# save the model
save(model, file = "svm_model3.RData")
# Load the model later with:
#load("svm_model.RData")
