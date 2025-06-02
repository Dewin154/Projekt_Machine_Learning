library(tree)

summary(data)

# Model calculation on the training data:
tree_model <- tree(price_in_euro ~ power_kw + transmission_type + mileage_in_km + sporty + age_in_months + fuel_type_new + brand_type, data=data.train_and_val)
tuning <- cv.tree(tree_model, K=5)
t <- which.min(tuning$dev)
number_of_endpoints <- tuning$size[t]

model <- prune.tree(tree_model, best=number_of_endpoints)
plot(model)
text(model)

# Calculation of prediction results on the test data:
X.test <- data.test[,c("power_kw", "transmission_type", "mileage_in_km", "sporty", "age_in_months", "fuel_type_new", "brand_type")]
prognosen <- predict(model, X.test)

# Calculation of the mean absolute error (MAE)
y.test <- data.test[,"price_in_euro"]
mean(abs(y.test - prognosen))

