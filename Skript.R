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

# removes a column in data with name "X"
data <- data[, !names(data) %in% "offer_description"]
data <- data[, !names(data) %in% "Unnamed..0"]
data <- data[, !names(data) %in% "power_ps"]
data <- data[, !names(data) %in% "fuel_consumption_g_km"]
data <- data[, !names(data) %in% "fuel_consumption_l_100km"]

# returns extensive summary with more levels
summary(data$color,maxsum=100)

# removes all NA lines
data <- data[!apply(is.na(data), 1, all), ]

#================================================================================
# OFFER_DESCRIPTION Sporty
data$sporty <- as.integer(grepl("quattro|xDrive|AWD|4WD|sport|GT|s-line", data$offer_description, ignore.case = TRUE))
summary(data$sporty)

# boxplot of the price_in_euro by sporty
boxplot(price_in_euro ~ sporty, data = data, 
    main = "Price in Euro by sporty", 
    xlab = "sporty", ylab = "Price in Euro", 
    col = c("lightblue", "lightgreen"))
#================================================================================
# COLOR

install.packages("dplyr")

# Create order based on median, do not execute if you don't want ordered boxplots!
library(dplyr)

data <- data %>%
  group_by(color) %>%
  mutate(median_price = median(price_in_euro, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(color = factor(color, levels = names(sort(tapply(price_in_euro, color, median, na.rm = TRUE)))))

# plot the boxplots for comparsion
boxplot(price_in_euro ~ color, data = data,
        main = "Price in Euro by color",
        xlab = "Color", ylab = "Price in Euro",
        las = 2, col = "lightblue")


#===============================================================================
# BRAND


library(dplyr)

data <- data %>%
  group_by(brand) %>%
  mutate(median_price = median(price_in_euro, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(brand = factor(brand, levels = names(sort(tapply(price_in_euro, brand, median, na.rm = TRUE)))))

boxplot(price_in_euro ~ brand, data = data,
        main = "Price in Euro by brand",
        xlab = "Brand", ylab = "Price in Euro",
        las = 2, col = "lightblue")

abline(h = 29400, col = "red", lwd = 2, lty = 1)
abline(h = 21000, col = "red", lwd = 2, lty = 1)



boxplot(price_in_euro ~ brand_type, data = data,
        main = "Price in Euro by brand_type",
        xlab = "Brand_type", ylab = "Price in Euro",
        las = 2, col = "lightblue")
    

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

#================================================================================
# Convert registration_date to age in months
library(lubridate)

# Define the target date (e.g., today's date or a specific date)
target_date <- as.Date("2023-09-01")

# Convert registration_date to Date format
data$registration_date <- as.Date(paste0("01/", data$registration_date), format = "%d/%m/%Y")

# Calculate the age in months
data$age_in_months <- interval(data$registration_date, target_date) %/% months(1)

# Display the first few rows to verify
head(data$age_in_months)

# diagram of age_in_months
par(mfrow = c(1,2))
boxplot(data$age_in_months,main="age_in_months")
hist(data$age_in_months, main="age_in_months")

# Filter out rows where 'age_in_months' is not between 0 and 240
data <- data[data$age_in_months >= 0 & data$age_in_months <= 240, ]

# test Square root transformation
data$age_in_months_sqrt <- sqrt(data$age_in_months)
par(mfrow = c(1, 2))
boxplot(data$age_in_months_sqrt,main="age_in_months_sqrt")
hist(data$age_in_months_sqrt, main="age_in_months_sqrt")
# Calculate correlation between 'age_in_months_sqrt' and 'price_in_euro'
correlation_age_months_price <- cor(data$age_in_months_sqrt, data$price_in_euro, use = "complete.obs")
# Create a scatter plot
par(mfrow = c(1, 1))
plot(data$age_in_months_sqrt, data$price_in_euro, 
    main = paste("Correlation between Age (sqrt) and Price in Euro: ", round(correlation_age_months_price, 2)),
    xlab = "Age (sqrt)", ylab = "Price in Euro", pch = 16, col = rgb(0, 0, 1, 0.5))

#===============================================================================
# FUEL_TYPE
# Combine fuel types into specified categories
data$fuel_type <- as.character(data$fuel_type)
data$fuel_type_new <- ifelse(data$fuel_type %in% c("Petrol", "Diesel"), data$fuel_type, "Others")
data$fuel_type_new <- as.factor(data$fuel_type_new)
summary(data$fuel_type_new)
# Display the distribution of fuel types

# Correlation between fuel_type and price_in_euro
boxplot(price_in_euro ~ fuel_type_new, data = data, 
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
# Filter out rows where 'year' is not between 2003 and 2023
data <- data[data$year >= 2003 & data$year <= 2023, ]
par(mfrow = c(1,2))
boxplot(data$year,main="year")
hist(data$year, main="year")
summary(data$year)

# Calculate correlation between 'year' and 'price_in_euro'
correlation <- cor(data$year, data$price_in_euro, use = "complete.obs")


# Create a scatter plot
par(mfrow = c(1, 1))
plot(data$year, data$price_in_euro, 
    main = paste("Correlation between Year and Price in Euro: ", round(correlation, 2)),
    xlab = "Year", ylab = "Price in Euro", pch = 16, col = rgb(0, 0, 1, 0.5))

# calculate age in years
data$age <- 2023 - data$year

# test log transformation
data$age_log <- log(data$age + 1) # Adding 1 to avoid log(0)
par(mfrow = c(1, 2))
boxplot(data$age_log,main="age_log")
hist(data$age_log, main="age_log")
# Calculate correlation between 'age_log' and 'price_in_euro'
correlation_age_log_price <- cor(data$age_log, data$price_in_euro, use = "complete.obs")
# Create a scatter plot
par(mfrow = c(1, 1))
plot(data$age_log, data$price_in_euro, 
    main = paste("Correlation between Age (log) and Price in Euro: ", round(correlation_age_log_price, 2)),
    xlab = "Age (log)", ylab = "Price in Euro", pch = 16, col = rgb(0, 0, 1, 0.5))



#================================================================================
# MILEAGE_IN_KM
# Display the 10 smallest and largest values of 'mileage_in_km'
cat("10 smallest values of mileage_in_km:\n")
print(sort(data$mileage_in_km, na.last = NA)[1:10])
cat("10 largest values of mileage_in_km:\n")
print(sort(data$mileage_in_km, decreasing = TRUE, na.last = NA)[1:10])


par(mfrow = c(1,2))
boxplot(data$mileage_in_km,main="mileage_in_km")
hist(data$mileage_in_km, main="mileage_in_km")


# Filter out rows where 'mileage_in_km' is not between 0 and 500000
data <- data[data$mileage_in_km >= 0 & data$mileage_in_km <= 500000, ]
par(mfrow = c(1,2))
boxplot(data$mileage_in_km,main="mileage_in_km")
hist(data$mileage_in_km, main="mileage_in_km")


# Calculate correlation between 'mileage_in_km' and 'price_in_euro'
correlation_mileage_price <- cor(data$mileage_in_km, data$price_in_euro, use = "complete.obs")
# Create a scatter plot
par(mfrow = c(1, 1))
plot(data$mileage_in_km, data$price_in_euro, 
    main = paste("Correlation between Mileage and Price in Euro: ", round(correlation_mileage_price, 2)),
    xlab = "Mileage in km", ylab = "Price in Euro", pch = 16, col = rgb(0, 0, 1, 0.5))  

# test Square root transformation
data$mileage_in_km_sqrt <- sqrt(data$mileage_in_km)
par(mfrow = c(1, 2))
boxplot(data$mileage_in_km_sqrt,main="mileage_in_km_sqrt")
hist(data$mileage_in_km_sqrt, main="mileage_in_km_sqrt")
# Calculate correlation between 'mileage_in_km_sqrt' and 'price_in_euro'
correlation_mileage_price_sqrt <- cor(data$mileage_in_km_sqrt, data$price_in_euro, use = "complete.obs")
# Create a scatter plot
par(mfrow = c(1, 1))
plot(data$mileage_in_km_sqrt, data$price_in_euro, 
    main = paste("Correlation between Mileage (sqrt) and Price in Euro: ", round(correlation_mileage_price_sqrt, 2)),
    xlab = "Mileage (sqrt)", ylab = "Price in Euro", pch = 16, col = rgb(0, 0, 1, 0.5))

summary(data$mileage_in_km)



#================================================================================
# POWER_KW
# Display the 10 smallest and largest values of 'power_kw'
cat("10 smallest values of power_kw:\n")
print(sort(data$power_kw, na.last = NA)[1:10])
cat("10 largest values of power_kw:\n")
print(sort(data$power_kw, decreasing = TRUE, na.last = NA)[1:10])
par(mfrow = c(1,2))
boxplot(data$power_kw,main="power_kw")
hist(data$power_kw, main="power_kw")
# Filter out rows where 'power_kw' is not between 30 and 300
data <- data[data$power_kw >= 30 & data$power_kw <= 300, ]
par(mfrow = c(1,2))
boxplot(data$power_kw,main="power_kw")
hist(data$power_kw, main="power_kw")
# Calculate correlation between 'power_kw' and 'price_in_euro'
correlation_power_price <- cor(data$power_kw, data$price_in_euro, use = "complete.obs")
# Create a scatter plot
par(mfrow = c(1, 1))
plot(data$power_kw, data$price_in_euro, 
    main = paste("Correlation between Power (kW) and Price in Euro: ", round(correlation_power_price, 2)),
    xlab = "Power (kW)", ylab = "Price in Euro", pch = 16, col = rgb(0, 0, 1, 0.5))

# test Square root transformation
data$power_kw_sqrt <- sqrt(data$power_kw)
par(mfrow = c(1, 2))
boxplot(data$power_kw_sqrt,main="power_kw_sqrt")
hist(data$power_kw_sqrt, main="power_kw_sqrt")
# Calculate correlation between 'power_kw_sqrt' and 'price_in_euro'
correlation_power_price_sqrt <- cor(data$power_kw_sqrt, data$price_in_euro, use = "complete.obs")
# Create a scatter plot
par(mfrow = c(1, 1))
plot(data$power_kw_sqrt, data$price_in_euro, 
    main = paste("Correlation between Power (sqrt) and Price in Euro: ", round(correlation_power_price_sqrt, 2)),
    xlab = "Power (sqrt)", ylab = "Price in Euro", pch = 16, col = rgb(0, 0, 1, 0.5))
summary(data$power_kw)
#================================================================================
# registration_date
# should be converted in months

#================================================================================
# FUEL TYPE L/100 KM

# Convert fuel_consumption_l_100km to numeric without "l/100km" and calculate the correlation with price
data$fuel_consumption_l_100km <- gsub(",", ".", data$fuel_consumption_l_100km)        
data$fuel_consumption_l_100km <- gsub(" l/100 km", "", data$fuel_consumption_l_100km)     
data$fuel_consumption_l_100km <- as.numeric(data$fuel_consumption_l_100km)  
correlation_fuel_price <- cor(data$fuel_consumption_l_100km, data$price_in_euro, use = "complete.obs")

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

#================================================================================
# Check for Mulitcollinearity

library(car)

install.packages("car")

linear_model <- lm(data$price_in_euro ~ data$power_kw + data$transmission_type + data$mileage_in_km + data$sporty + data$age_in_months + data$fuel_type_new + data$brand_type, data = data)

vif_values <- car::vif(linear_model)
print(vif_values)

# VIF = 1, no multicollinearity
# VIF 1 to 5, moderate multicollinearity, not a problem but keep the variables in mind
# VIF 5 to 10, severe multicollinearity, think about steps to reduce multicollinearity
# VIF > 10, extreme multicollinearity, delete the variable 

data_raw <- read.csv("data.csv", header=TRUE, sep=",", fill=TRUE, stringsAsFactors = TRUE)

summary(data_raw)

# calculate frequencies
brand_counts <- summary(data_raw$brand)

# sort frequencies
brand_counts_sorted <- sort(brand_counts, decreasing = TRUE)

# Histogramm of the frequencies
barplot(brand_counts_sorted,
        las = 2,                
        cex.names = 0.8,        
        main = "Häufigkeit der Marken",
        xlab = "Marke",
        ylab = "Anzahl",
        col = "steelblue")

# calculate frequencies
color_counts <- summary(data_raw$color)

# sort frequencies
color_counts_sorted <- sort(color_counts, decreasing = TRUE)

# Histogramm of the frequencies
barplot(color_counts_sorted,
        las = 2,                
        cex.names = 0.8,        
        main = "Häufigkeit der Farben",
        xlab = "Farbe",
        ylab = "Anzahl",
        col = "steelblue")

data_raw$year <- as.numeric(gsub(",", ".", as.character(data_raw$year)))
data_raw$price_in_euro <- as.numeric(gsub(",", ".", as.character(data_raw$price_in_euro)))
data_raw$mileage_in_km <- as.numeric(gsub(",", ".", as.character(data_raw$mileage_in_km)))

mean(data_raw$year, na.rm = TRUE)
mean(data_raw$price_in_euro, na.rm = TRUE)

boxplot(data_raw$price_in_euro)

plot(data_raw$price_in_euro, data_raw$mileage_in_km,
     xlab = "Preis in Euro", ylab = "Kilometerstand",
     main = "Preis und Kilometerstand")


summary(data)
mean(data$price_in_euro, na.rm = TRUE)

plot(data$price_in_euro, data$mileage_in_km,
     xlab = "Preis in Euro", ylab = "Kilometerstand",
     main = "Preis und Kilometerstand")


# Only years up to 2024
filtered_years <- data_raw$year[data_raw$year <= 2024]

# Frequencies of the years
year_counts <- table(filtered_years)

# Sort years
year_counts <- sort(year_counts)
