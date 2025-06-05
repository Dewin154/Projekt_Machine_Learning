library(randomForest)

# Modelltraining mit Random Forest auf den Trainings- und Validierungsdaten
rf_model <- randomForest(
  price_in_euro ~ power_kw + transmission_type + mileage_in_km + sporty + age_in_months + fuel_type_new + brand_type,
  data = data.train_and_val,
  ntree = 50,         # Anzahl der Bäume (Standard: 500)
  importance = TRUE    # Variable Importance berechnen
)

# Vorhersage auf den Testdaten
X.test <- data.test[, c("power_kw", "transmission_type", "mileage_in_km", "sporty", "age_in_months", "fuel_type_new", "brand_type")]
prognosen_rf <- predict(rf_model, X.test)

# Berechnung des MAE
y.test <- data.test[,"price_in_euro"]
mae_rf <- mean(abs(y.test - prognosen_rf))
print(mae_rf)

# Variable Importance Plot
varImpPlot(rf_model)
