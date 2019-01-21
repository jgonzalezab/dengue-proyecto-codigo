# Load country equivalence
countryEquivalence <- read.csv('equivalencesCountry.csv')
countryEquivalence$ISO3166Alpha2 <- as.character(countryEquivalence$ISO3166Alpha2)
countryEquivalence$ISO3166Alpha3 <- as.character(countryEquivalence$ISO3166Alpha3)
countryEquivalence$Country <- as.character(countryEquivalence$Country)

# Load data
data1 <- read.csv('W_Table_data_digested.csv')
data1$Country <- as.character(data1$Country)
data1$ISO3166Alpha2 <- as.character(data1$ISO3166Alpha2)
data1$ISO3166Alpha3 <- as.character(data1$ISO3166Alpha3)

# Reorder and rename the columns
data1 <- data1[, c(6, 1, 8, 3, 4, 7)]
names(data1)[1:3] <- c('Country_Code', 'Country', 'Year')

# Collapse by country and year
data1 <- aggregate(formula = .~ Country_Code + Country + Year,
                   data = data1, FUN = sum)

# Order by country
data1 <- data1[order(data1$Country_Code), ]

# Assign ID
data1 <- cbind.data.frame(ID = 1:nrow(data1),
                          data1)

# Save CSV
write.csv(file = 'datos-casos-dengue-paho.csv', data1, row.names = FALSE)
