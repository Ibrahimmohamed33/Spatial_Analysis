library(dplyr)
library(ggplot2)
library(reshape2)
library(rgeos)
library(rgdal)
library(tmap)
library(leaflet)
library(RColorBrewer)

dir <- '~/Dropbox/Downloads/Camden'
setwd(dir)

Ethinicity <- read.csv('~/Dropbox/Downloads/Camden/tables/KS201EW_oa11.csv')
Rooms <- read.csv('~/Dropbox/Downloads/Camden/tables/KS403EW_oa11.csv')
Qualifications <- read.csv('~/Dropbox/Downloads/Camden/tables/KS501EW_oa11.csv')
Employment <- read.csv('~/Dropbox/Downloads/Camden/tables/KS601EW_oa11.csv')

Ethinicity <- Ethinicity[ , c(1,21)]
Rooms <- Rooms[ , c(1,13)]
Employment <- Employment[ , c(1,20)]
Qualifications <- Qualifications[ , c(1,20)]

names(Ethinicity) <- c('OA', 'White British')
names(Rooms) <- c('OA', 'Low_Occupancy')
names(Employment) <- c('OA', 'Unemployed')
names(Qualifications) <- c('OA', 'Qualification')

m1 <- merge(Ethinicity, Rooms, by = 'OA')
m2 <- merge(m1, Employment, by = 'OA')
Census.Data <- merge(m2, Qualifications, by = 'OA')
rm(m1, m2)

write.csv(Census.Data, 'practical_data.csv', row.names = F) ##End of Chapter 1

Census.Data <- read.csv('practical_data.csv')

#The rest of the code will consist of many visual plots and statistical analyses

hist(Census.Data$Unemployed, breaks = 100, col = 'Red', main = 'Percentage in 
     Full-Time Employment', xlab = 'Percentage')

boxplot(Census.Data[,2:5])

plot(Census.Data$Unemployed, Census.Data$Qualification, col = 'Red', 
     xlab = "Unemployment Rate", ylab = 'Qualification Rate')

symbols(Census.Data$Unemployed, Census.Data$Qualification, 
        circles = Census.Data$White.British, 
        fg = 'Black', bg = 'Purple', inches = 0.200,
        xlab = "Unemployment Rate", ylab = "Qualifcation Rate",
        main = "Proportion of White British")

abline(lm(Census.Data$Qualification ~ Census.Data$Unemployed), col = 'Black',
       lty = 1, lwd = 3)

p <- ggplot(Census.Data, aes(Unemployed, Qualification))
p + geom_point(aes(colour = White.British, size = Low_Occupancy))

cor.test(Census.Data$Unemployed, Census.Data$Qualification, method = "spearman")

round(cor(Census.Data[, 2:5]), 2)

qplot(x = Var1, y = Var2, data = melt(cor(Census.Data[, 2:5], use = 'p')), fill = value, 
      geom = 'tile') + scale_fill_gradient2(limits = c(-1,1))

plot(Census.Data$Unemployed, Census.Data$Qualification, type = 'p',
     xlab = "Unemployment Rate", ylab = '% Without a Qualification') +
        abline(lm(Census.Data$Qualification ~ Census.Data$Unemployed))

summary(lm(Census.Data$Qualification ~ Census.Data$Unemployed))


Output.Areas <- readOGR('.', 'Camden_oa11')
OA.Census <- merge(Output.Areas, Census.Data, by.x = 'OA11CD', by.y = 'OA')
proj4string(OA.Census) <- CRS("+init=EPSG:27700")
#qtm(OA.Census, fill = 'Qualification')
tm_shape(OA.Census) + tm_borders(alpha = .4) + tm_fill('Qualification', style = 'pretty', 
                              n = 4, palette = 'Reds', legend.hist = TRUE) 


tm_shape(OA.Census) + tm_borders(alpha = .3, lty = 'solid') +
  tm_fill('Qualification', palette = 'Reds', style = 'pretty', 
          title = '% with a Qualification') +
tm_layout(title = 'Camden, London', legend.text.size = 1.1, 
          legend.title.size = 1.1, legend.position = c('right', 'top'), 
          frame = FALSE)

writeOGR(OA.Census, dsn = '~/Dropbox/Downloads/Camden', 
         layer = 'Census_OA_Shapefile', driver = 'ESRI Shapefile')
