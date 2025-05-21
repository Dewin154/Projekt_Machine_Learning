# Run this script to clean the data set to be up to date

# read data from the file
data <- read.csv("data.csv", header=TRUE, sep=",", fill=TRUE, stringsAsFactors = TRUE)

# convert the variables to the correct type
data$year <- as.numeric(gsub(",", ".", as.character(data$year)))
data$price_in_euro <- as.numeric(gsub(",", ".", as.character(data$price_in_euro)))
data$power_kw <- as.numeric(gsub(",", ".", as.character(data$power_kw)))
data$power_ps <- as.numeric(gsub(",", ".", as.character(data$power_ps)))
data$mileage_in_km <- as.numeric(gsub(",", ".", as.character(data$mileage_in_km)))

summary(data)

# remove columns with the name of "X"
data <- data[, !names(data) %in% "Unnamed..0"]
data <- data[, !names(data) %in% "power_ps"]
data <- data[, !names(data) %in% "fuel_consumption_g_km"]
data <- data[, !names(data) %in% "fuel_consumption_l_100km"]


# remove all unknown entries in transmission type and drop the "Unknown" category as a level
data <- data[data$transmission_type != "Unknown", ]
data$transmission_type <- droplevels(data$transmission_type)


# adds new column if the car is sport car and convert it to factor
data$sporty <- as.integer(grepl("quattro|xDrive|AWD|4WD|sport|GT|s-line", data$offer_description, ignore.case = TRUE))
data$sporty <- as.factor(data$sporty)

# Limit the price of the cars to 60.000 Euro
data <- data[!is.na(data$price_in_euro) & data$price_in_euro <= 60000, ]

library(lubridate)

# Define the target date (e.g., today's date or a specific date)
target_date <- as.Date("2023-09-01")

# Convert registration_date to Date format
data$registration_date <- as.Date(paste0("01/", data$registration_date), format = "%d/%m/%Y")

# Calculate the age in months
data$age_in_months <- interval(data$registration_date, target_date) %/% months(1)

# Filter out rows where 'age_in_months' is not between 0 and 240
data <- data[data$age_in_months >= 0 & data$age_in_months <= 240, ]

# fuel_type
# Combine fuel types into specified categories
data$fuel_type <- as.character(data$fuel_type)
data$fuel_type_new <- ifelse(data$fuel_type %in% c("Petrol", "Diesel"), data$fuel_type, "Others")
data$fuel_type_new <- as.factor(data$fuel_type_new)

# Filter out rows where 'year' is not between 2003 and 2023
data <- data[data$year >= 2003 & data$year <= 2023, ]

# Filter out rows where 'mileage_in_km' is not between 0 and 500000
data <- data[data$mileage_in_km >= 0 & data$mileage_in_km <= 500000, ]

# Filter out rows where 'power_kw' is not between 30 and 300
data <- data[data$power_kw >= 30 & data$power_kw <= 300, ]

# Now "Offer_description", "registration_date" etc. can be removed, since data from them has been extracted and processed
data <- data[, !names(data) %in% "offer_description"]
data <- data[, !names(data) %in% "registration_date"]
data <- data[, !names(data) %in% "fuel_type"]

# removes all NA lines after all conversions
data <- data[!apply(is.na(data), 1, all), ]

# after we anaylized the influence of color on price, we can remove it
data <- data[, !names(data) %in% "color"]
data <- data[, !names(data) %in% "median_price"]


data$brand_type <- ifelse(data$brand %in% c("daewoo", "daihatsu", "lancia", "lada", "fiat", "dacia", "ford", "citroen", "mazda","hyundai", "bmw", "infiniti", "kia", "honda", "audi"), "standard", "premium")
data$brand_type <- as.factor(data$brand_type)

data <- data[, !names(data) %in% "brand"]
data <- data[, !names(data) %in% "model"]

summary(data)
