library(ANN2)

X <- model.matrix(
  price_in_euro ~ power_kw + transmission_type + mileage_in_km + sporty + age_in_months + fuel_type_new + brand_type,
  data.train_and_val
)

X <- X[, -1]
summary(X)

y <- data.train_and_val[, "price_in_euro"]

model <- neuralnetwork(
  X,
  y,
  hidden.layers = c(4, 3),
  regression = TRUE,
  loss.type = "absolute",
  learn.rates = 1e-04,
  n.epochs = 50,
  verbose = FALSE
)
plot(model)

# Berechnung der Prognosen auf den Testdaten:
# Achtung: im Befehl 'model.matrix' steht bei 'data' nun Daten.test !!!

X.test <- model.matrix(price_in_euro ~ power_kw + transmission_type + mileage_in_km + sporty + age_in_months + fuel_type_new + brand_type,
                       data.test)
X.test <- X.test[, -1]
prognosen <- predict(model, X.test)$predictions
y.test <- data.test[, "price_in_euro"]
mean(abs(y.test - prognosen))
