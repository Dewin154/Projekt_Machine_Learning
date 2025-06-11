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

mean(data_raw$year, na.rm = TRUE)


