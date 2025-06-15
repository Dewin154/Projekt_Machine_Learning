library(ANN2)
library(caret)

set.seed(42)

X <- model.matrix(
  price_in_euro ~ power_kw + transmission_type + mileage_in_km + sporty + age_in_months + fuel_type_new + brand_type,
  data.train_and_val
)
X <- X[, -1]
y <- data.train_and_val[, "price_in_euro"]

train_data <- as.data.frame(X)
train_data$price_in_euro <- y

# 10-fold Crossvalidation
ctrl <- trainControl(method = "cv", number = 10)

# Neurons of 1st Hidden layer as parameter
grid <- expand.grid(hidden1 = c(3, 5, 7))

# Trainingsfunction forcaret
caret_ann2 <- list(
  type = "Regression",
  library = "ANN2",
  parameters = data.frame(parameter = "hidden1", class = "numeric", label = "Hidden Layer 1"),
  grid = function(x, y, len = NULL, search = "grid") grid,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    neuralnetwork(
      x, y,
      hidden.layers = c(param$hidden1, 3),
      regression = TRUE,
      loss.type = "absolute",
      learn.rates = 1e-04,
      n.epochs = 30,
      verbose = FALSE
    )
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    as.vector(predict(modelFit, as.matrix(newdata))$predictions)
  },
  prob = NULL
)

# Training with caret
model <- train(
  price_in_euro ~ .,
  data = train_data,
  method = caret_ann2,
  trControl = ctrl,
  tuneGrid = grid,
  metric = "MAE"
)
print(model)

# Prognose for test data
X.test <- model.matrix(price_in_euro ~ power_kw + transmission_type + mileage_in_km + sporty + age_in_months + fuel_type_new + brand_type,
                       data.test)
X.test <- X.test[, -1]
prognosen <- predict(model, as.data.frame(X.test))
y.test <- data.test[, "price_in_euro"]
mean(abs(y.test - prognosen))

plot(model)

save(model, file = "neural_network.RData")

