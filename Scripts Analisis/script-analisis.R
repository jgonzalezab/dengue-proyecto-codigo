# Needed libraries
library(zoo)
library(ggplot2)

# Load data
dataDengue <- read.csv('dengue-mosquitos-casosDengue.csv')
dataDengue$Country_Code <- as.character(dataDengue$Country_Code)
dataDengue$Country <- as.character(dataDengue$Country)

########################### Defining useful functions to apply to the data ###########################
# Plot the number of mosquitos obervations for a specific country
# Arguments:
#   - Country_Code: Country cases to plot
#   - dataDF: Data used (only dataDengue available)
#   - source: Source of the cases (Dryad or GBIF)
#   - interpolate: Whether to interpolate or not (linear interpolation)
plotObsv <- function(Country_Code, dataDF = dataDengue,
                     source = 'Dryad',
                     interpolate = TRUE) {
  
  aux <- dataDF[dataDF$Country_Code == Country_Code, ]
  switch(tolower(source),
         dryad = {
           toPlot <- aux[, c(3, 4)]
         },
         gbif = {
           toPlot <- aux[, c(3, 5)] 
         },
         {
           print('Wrong source provided')
         })
  
  if(interpolate) {
    toPlot[, 2] <- na.approx(toPlot[, 2], na.rm = FALSE)
  }
  
  p <- ggplot() +
    geom_line(aes(x = toPlot$Year, y = toPlot[, 2]),
              colour = 'blue', size = 1.3) +
    xlab('Year') + ylab('Number of observations') +
    ggtitle(paste('Mosquito observations for country with code', Country_Code))
  suppressWarnings(print(p))
    
}

# Plot the number of dengue cases for a specific country
# Arguments:
#   - Country_Code: Country cases to plot
#   - dataDF: Data used (only dataDengue available)
#   - Denguetype: Three available choices (you can combine them)
#                 D -> Dengue
#                 DG -> Dengue Grave
#                 M -> Muertes
#   - interpolate: Whether to interpolate or not (linear interpolation)
plotDengue <- function(Country_Code, dataDF = dataDengue,
                       type = c('D', 'DG', 'M'),
                       interpolate = TRUE) {
  
  aux <- dataDF[dataDF$Country_Code == Country_Code, ]
  
  if(interpolate) {
    aux$Dengue <- na.approx(aux$Dengue, na.rm = FALSE)
    aux$Dengue.Grave <- na.approx(aux$Dengue.Grave, na.rm = FALSE)
    aux$Muertes <- na.approx(aux$Muertes, na.rm = FALSE)
  }
  
  p <- ggplot()
  
  if('D' %in% type) {
    p <- p +
      geom_line(aes(x = aux$Year, y = aux$Dengue,
                    colour = 'Dengue'), size = 1.3)
  }
  
  if('DG' %in% type) {
    p <- p + 
      geom_line(aes(x = aux$Year, y = aux$Dengue.Grave,
                    colour = 'Dengue Grave'), size = 1.3)
  }
  
  if('M' %in% type) {
    p <- p +
      geom_line(aes(x = aux$Year, y = aux$Muertes,
                    colour = 'Muertes'), size = 1.3)
  }
  
  p <- p + xlab('Year') + ylab('Number of Cases') +
    ggtitle(paste('Cases for country with code', Country_Code)) +
    theme(legend.title = element_blank())
  suppressWarnings(print(p))
  
}
#######################################################################################################
# Comparison Brazil Dryad and GBIF
png(filename = 'BRA_Dryad.png',
    width = 800, height = 500)
plotObsv('BRA', source = 'Dryad')
dev.off()

png(filename = 'BRA_GBIF.png',
    width = 800, height = 500)
plotObsv('BRA', source = 'GBIF')
dev.off()

# Comparison USA Dryad and GBIF
png(filename = 'USA_Dryad.png',
    width = 800, height = 500)
plotObsv('USA', source = 'Dryad')
dev.off()

png(filename = 'USA_GBIF.png',
    width = 800, height = 500)
plotObsv('USA', source = 'GBIF')
dev.off()

# Comparisons observations Brazil vs USA
png(filename = 'BRA_OBSV.png',
    width = 800, height = 500)
plotObsv('BRA', source = 'Dryad')
dev.off()

png(filename = 'USA_OBSV.png',
    width = 800, height = 500)
plotObsv('USA', source = 'Dryad')
dev.off()

# Comparison Dengue Brazil vs USA
png(filename = 'BRA_D.png',
    width = 800, height = 500)
plotDengue('BRA', type = 'D')
dev.off()

png(filename = 'USA_D.png',
    width = 800, height = 500)
plotDengue('USA', type = 'D')
dev.off()

# Correlation to justify the previous plot
BRA_USA_D <- merge(dataDengue[dataDengue$Country_Code == 'BRA', c(3, 6)],
                   dataDengue[dataDengue$Country_Code == 'USA', c(3, 6)],
                   by = c('Year'))
BRA_USA_D <- BRA_USA_D[-1, ]
paste('Correlation:', cor(BRA_USA_D$Dengue.x,
                          BRA_USA_D$Dengue.y))

# Comparison Dengue Grave Brazil vs USA
png(filename = 'BRA_DG.png',
    width = 800, height = 500)
plotDengue('BRA', type = 'DG')
dev.off()

png(filename = 'USA_DG.png',
    width = 800, height = 500)
plotDengue('USA', type = 'DG')
dev.off()

# Correlation to justify the previous plot
BRA_USA_DG <- merge(dataDengue[dataDengue$Country_Code == 'BRA', c(3, 7)],
                    dataDengue[dataDengue$Country_Code == 'USA', c(3, 7)],
                   by = c('Year'))
BRA_USA_DG <- BRA_USA_DG[-1, ]
paste('Correlation:', cor(BRA_USA_DG$Dengue.Grave.x,
                          BRA_USA_DG$Dengue.Grave.y))

# Comparison Muertes Brazil y USA
png(filename = 'BRA_M.png',
    width = 800, height = 500)
plotDengue('BRA', type = 'M')
dev.off()

png(filename = 'USA_M.png',
    width = 800, height = 500)
plotDengue('USA', type = 'M')
dev.off()
