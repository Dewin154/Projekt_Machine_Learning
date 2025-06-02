# install.packages("e1071")
library(e1071)

cc <- seq(-5,10,1)    # für mögliche Werte von "Cost" (Tuningparameter)
cg <- seq(-3,1,0.5)   # für mögliche werte von "gamma" (Tuningparameter)

tuning <- tune.svm(price_in_euro ~ power_kw + transmission_type + mileage_in_km + sporty + age_in_months + fuel_type_new + brand_type,
                   data=data.train_and_val,
                   scale = TRUE, type = "eps-regression", kernel = "radial",
                   gamma = 10^cg, cost = 2^cc, epsilon = 0.1,
                   tunecontrol = tune.control(sampling = "cross",cross=5))

print(tuning)
model <- tuning$best.model    # das Model mit optimalen Tuningparametern

# Calculation of prediction results on the test data:
X.test <- data.test[,c("power_kw", "transmission_type", "mileage_in_km", "sporty", "age_in_months", "fuel_type_new", "brand_type")]
prognosen <- predict(model, X.test)

# Calculation of the mean absolute error (MAE)
y.test <- data.test[,"price_in_euro"]
mean(abs(y.test - prognosen))
