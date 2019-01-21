# Load country equivalence
countryEquivalence <- read.csv('equivalencesCountry.csv')
countryEquivalence$ISO3166Alpha2 <- as.character(countryEquivalence$ISO3166Alpha2)
countryEquivalence$ISO3166Alpha3 <- as.character(countryEquivalence$ISO3166Alpha3)
countryEquivalence$Country <- as.character(countryEquivalence$Country)

# Load data
data1 <- read.csv('0025895-181108115102211_digested.csv')
data1$countryCode <- as.character(data1$countryCode)

# Create the necessary columns
data1 <- cbind.data.frame(Country_Code = data1$countryCode,
                          Country = rep(NA, nrow(data1)),
                          Year = data1$year)
data1$Country_Code <- as.character(data1$Country_Code)

# Change the format in the country codes and assign country names
toDelete <- c()
for(i in 1:nrow(data1)) {
  
  if(data1$Country_Code[i] %in% countryEquivalence$ISO3166Alpha2) {
    
    data1$Country_Code[i] <- countryEquivalence$ISO3166Alpha3[countryEquivalence$ISO3166Alpha2 == data1$Country_Code[i]]
    data1$Country[i] <- countryEquivalence$Country[countryEquivalence$ISO3166Alpha3 == data1$Country_Code[i]]
    
  } else {
    
    toDelete <- c(toDelete, i)
    
  }
  
}

# Remove not america's cases
data1 <- data1[-toDelete, ]

# Collapse by country and year
data1 <- cbind.data.frame(data1,
                          MosquitoCases = rep(1, nrow(data1)))
data1 <- aggregate(data1$MosquitoCases,
                   by = list(Country_Code = data1$Country_Code,
                             Country = data1$Country,
                             Year = data1$Year), FUN = sum)
names(data1)[ncol(data1)] <- 'MosquitoCases'

# Order by country
data1 <- data1[order(data1$Country_Code), ]

# Assign ID
data1 <- cbind.data.frame(ID = 1:nrow(data1),
                          data1)

# Save CSV
write.csv(file = 'datos-mosquito-gbif.csv', data1, row.names = FALSE)
