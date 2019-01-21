# Load data
mosquitosDryad <- read.csv('datos-mosquito-dryad.csv')
mosquitosGbif <- read.csv('datos-mosquito-gbif.csv')
casosDengue <- read.csv('datos-casos-dengue-paho.csv')

# Merge of the dataframes
finalData <- merge(mosquitosDryad[, -1], mosquitosGbif[, -1],
                   by = c('Country_Code', 'Country', 'Year'),
                   all = TRUE)
names(finalData)[c(4, 5)] <- c('MosquitosCases_Dryad',
                               'MosquitosCases_GBIF')

finalData <- merge(finalData, casosDengue[, -1],
                   by = c('Country_Code', 'Country', 'Year'),
                   all = TRUE)

# Save the data
write.csv(file = 'dengue-mosquitos-casosDengue.csv',
          finalData, row.names = FALSE)
