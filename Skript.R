setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv("data.csv", header=TRUE, sep=",", fill=TRUE, stringsAsFactors = TRUE)

summary(data)

#counts the number of cars that have in description column the word "unfallfrei"
anzahl_unfallfrei <- sum(grepl("unfallfrei", data$offer_description))

#correct conversion of variables to numeric
data$year <- as.numeric(gsub(",", ".", as.character(data$year)))
data$price_in_euro <- as.numeric(gsub(",", ".", as.character(data$price_in_euro)))
data$power_kw <- as.numeric(gsub(",", ".", as.character(data$power_kw)))
data$power_ps <- as.numeric(gsub(",", ".", as.character(data$power_ps)))
data$mileage_in_km <- as.numeric(gsub(",", ".", as.character(data$mileage_in_km)))

# returns extensive summary with more levels
summary(data,maxsum=100)

# removes all NA lines
data <- data[!apply(is.na(data), 1, all), ]


# removes a column in data with name "X"
data <- data[, !names(data) %in% "offer_description"]
data <- data[, !names(data) %in% "Unnamed..0"]
data <- data[, !names(data) %in% "power_ps"]



# registration_date ignorieren, da es mit year korreliert

#===============================================================================
# PRICE in EURO
# Display the 10 smallest and largest values of 'price_in_euro'
cat("10 smallest values of price_in_euro:\n")
print(sort(data$price_in_euro, na.last = NA)[1:10])
cat("10 largest values of price_in_euro:\n")
print(sort(data$price_in_euro, decreasing = TRUE, na.last = NA)[1:10])

data <- data[!is.na(data$price_in_euro) & data$price_in_euro <= 60000, ]

par(mfrow = c(1,2))
boxplot(data$price_in_euro,main="price_in_euro")
hist(data$price_in_euro, main="price_in_euro")

#===============================================================================
# fuel_type
# Combine fuel types into specified categories
data$fuel_type <- as.character(data$fuel_type)
data$fuel_type <- ifelse(data$fuel_type %in% c("Petrol", "Diesel"), data$fuel_type, "Others")
data$fuel_type <- as.factor(data$fuel_type)

# Display the distribution of fuel types

# Correlation between fuel_type and price_in_euro
boxplot(price_in_euro ~ fuel_type, data = data, 
    main = "Price in Euro by Fuel Type", 
    xlab = "Fuel Type", ylab = "Price in Euro", 
    col = c("lightblue", "lightgreen", "lightcoral"))
#===============================================================================
# YEAR
par(mfrow = c(1,1))
boxplot(data$year,main="year")
# Display the 10 smallest and largest values of 'year'
cat("10 smallest values of year:\n")
print(sort(data$year, na.last = NA)[1:10])

cat("10 largest values of year:\n")
print(sort(data$year, decreasing = TRUE, na.last = NA)[1:10])
par(mfrow = c(1,2))
boxplot(data$year,main="year")
hist(data$year, main="year")
# Filter out rows where 'year' is not between 1995 and 2023
data <- data[data$year >= 1995 & data$year <= 2023, ]
par(mfrow = c(1,2))
boxplot(data$year,main="year")
summary(data$year)

# Calculate correlation between 'year' and 'price_in_euro'
correlation <- cor(data$year, data$price_in_euro, use = "complete.obs")
correlation_fuel_price <- cor(data$fuel_consumption_l_100km, data$price_in_euro, use = "complete.obs")

# Create a scatter plot
par(mfrow = c(1, 1))
plot(data$year, data$price_in_euro, 
    main = paste("Correlation between Year and Price in Euro: ", round(correlation, 2)),
    xlab = "Year", ylab = "Price in Euro", pch = 16, col = rgb(0, 0, 1, 0.5))

par(mfrow = c(1, 1))
plot(data$fuel_consumption_l_100km, data$price_in_euro, 
     main = paste("Correlation between Fuel and Log Price in Euro: ", round(correlation_fuel_price, 2)),
     xlab = "Fuel", ylab = "Log Price in Euro", pch = 16, col = rgb(0, 0, 1, 0.5))

# Add a trend line
abline(lm(data$price_in_euro ~ data$year, data = data), col = "red", lwd = 2)

#===============================================================================
# Convert 'fuel_consumption' to numeric by extracting the numeric part and converting it
data$fuel_consumption_l_100km <- as.numeric(gsub(",", ".", gsub(" l/100 km", "", data$fuel_consumption_l_100km)))

# Filter fuel above 25l
data <- data[data$fuel_consumption_l_100km <= 25, ]

# Display summary of the converted 'fuel_consumption' column
summary(data$fuel_consumption_l_100km)

# Visualize the distribution of 'fuel_consumption'
par(mfrow = c(1, 2))
boxplot(data$fuel_consumption_l_100km, main = "Fuel Consumption (l/100 km)")
hist(data$fuel_consumption_l_100km, main = "Fuel Consumption (l/100 km)", xlab = "Fuel Consumption (l/100 km)")
#===============================================================================
# registration_date
# should be converted in months

#================================================================================


# Verteilungen beobachten
par(mfrow = c(2,5))
#boxplot(data[,1],main=names(data)[1]) # nur ID
#boxplot(data[,2],main=names(data)[2])
#boxplot(data[,3],main=names(data)[3])
#boxplot(data[,4],main=names(data)[4])
#boxplot(data[,5],main=names(data)[5])
boxplot(data[,6],main=names(data)[6])
boxplot(data[,7],main=names(data)[7])
boxplot(data[,8],main=names(data)[8])
boxplot(data[,9],main=names(data)[9])
boxplot(data[,10],main=names(data)[10])
boxplot(data[,11],main=names(data)[11])
boxplot(data[,12],main=names(data)[12])
boxplot(data[,13],main=names(data)[13])
boxplot(data[,14],main=names(data)[14])
boxplot(data[,15],main=names(data)[15])
