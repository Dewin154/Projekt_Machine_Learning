print(data)

#setwd("C:/Users/peter/THD/4_Semester/Machine_Learning")
#print(dirname(rstudioapi::getActiveDocumentContext()$path))
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