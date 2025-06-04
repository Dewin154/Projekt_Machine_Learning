library(ANN2)

X <- model.matrix(
  price_in_euro ~ power_kw + transmission_type + mileage_in_km + sporty + age_in_months + fuel_type_new + brand_type,
  data.train_and_val
)
X <- X[, -1]
y <- data.train_and_val[, "price_in_euro"]

# Kreuzvalidierung mit ANN2
set.seed(42)
cv_res <- cv.neuralnetwork(
  X,
  y,
  hidden.layers = list(c(3, 3), c(5, 3), c(7, 3)), # nur erster Layer variiert
  regression = TRUE,
  loss.type = "absolute",
  learn.rates = 1e-04,
  n.epochs = 20,
  k.folds = 5,
  verbose = TRUE
)
print(cv_res)

# Optional: bestes Modell trainieren
best_hidden <- cv_res$best.hidden.layers
model <- neuralnetwork(
  X,
  y,
  hidden.layers = best_hidden,
  regression = TRUE,
  loss.type = "absolute",
  learn.rates = 1e-04,
  n.epochs = 50,
  verbose = FALSE
)
plot(model)

# Prognosen auf Testdaten
X.test <- model.matrix(price_in_euro ~ power_kw + transmission_type + mileage_in_km + sporty + age_in_months + fuel_type_new + brand_type,
                       data.test)
X.test <- X.test[, -1]
prognosen <- predict(model, X.test)$predictions
y.test <- data.test[, "price_in_euro"]
mean(abs(y.test - prognosen))
