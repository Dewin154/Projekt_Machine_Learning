library(tree)

summary(data)

# Aufteilen der Daten:
# Anzahl der Daten
set.seed(123)  # Für Reproduzierbarkeit
n <- nrow(data)
train_idx <- sample(seq_len(n), size = 0.7 * n)
data.train <- data[train_idx, ]
data.test <- data[-train_idx, ]

# Berechnung des Modells auf den Trainingsdaten:
# Achtung: im Befehl 'cv.tree' steht bei 'data' nun Daten.train !!!

tree_model <- tree(price_in_euro ~ power_kw + transmission_type + mileage_in_km + sporty + age_in_months + fuel_type_new + brand_type, data=data.train)
tuning <- cv.tree(tree_model, K=5)
t <- which.min(tuning$dev)
number_of_endpoints <- tuning$size[t]

model <- prune.tree(tree_model,best=number_of_endpoints)
plot(model)
text(model)


# Berechnung der Prognoseergebnisse auf den Testdaten:

X.test <- data.test[,c("power_kw", "transmission_type", "mileage_in_km", "sporty", "age_in_months", "fuel_type_new", "brand_type")]
prognosen <- predict(model,X.test)

# Berechnung des mittleren Prognosefehlers (MAE)

y.test <- data.test[,"price_in_euro"]
mean(abs(y.test-prognosen))

