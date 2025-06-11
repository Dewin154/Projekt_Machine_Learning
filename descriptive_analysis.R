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

