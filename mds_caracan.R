## 
## Mutidimensional Scaling Tutorial: smacof and data.table
## Author: Cara Cannella, caracan@umich.edu
##

# 80: -------------------------------------------------------------------------- 

# libraries: -------------------------------------------------------------------

library(MASS)
library(tidyverse)
library(ggplot2)
library(data.table)
library(smacof)

# 80: -------------------------------------------------------------------------- 

# Get data
Cars93 = as.data.table(MASS::Cars93)

Cars93 = Cars93[!is.na(Rear.seat.room) & !is.na(Luggage.room)]

Cars93_cont_scaled = Cars93[,.(Min.Price, Price, Max.Price,
                               MPG.city, MPG.highway, EngineSize,
                               Horsepower, RPM, Rev.per.mile,
                               Length, Wheelbase, Width,
                               Turn.circle, Rear.seat.room,
                               Luggage.room)]) %>% scale()

for (method in c('euclidean', 'manhattan', 'maximum')) {
  assign(paste("distance", method, sep='_'), dist(Cars93_cont_scaled, method = method))
}

distance_correlation = sqrt(2*(1-cor(t(Cars93_cont_scaled))))

# 80: -------------------------------------------------------------------------- 

# Euclidean Distance

torgerson_euclidean = torgerson(distance_euclidean)
df_euclidean = data.frame(
  Z1=torgerson_euclidean[,1], Z2=torgerson_euclidean[,2], 
  Model=Cars93$Model)
ggplot(df_euclidean, aes(x=Z1,y=Z2),main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

# 80: --------------------------------------------------------------------------

# Manhattan Distance

torgerson_manhattan = torgerson(distance_manhattan)
df_manhattan = data.frame(
  Z1=torgerson_manhattan[,1], Z2=torgerson_manhattan[,2], 
  Model=Cars93$Model)
ggplot(df_manhattan, aes(x=Z1,y=Z2),main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

# 80: --------------------------------------------------------------------------

# Maximum Distance 

torgerson_maximum = torgerson(distance_maximum)
df_maximum = data.frame(
  Z1=torgerson_maximum[,1], Z2=torgerson_maximum[,2], 
  Model=Cars93$Model)
ggplot(df_maximum, aes(x=Z1,y=Z2),main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

# 80: --------------------------------------------------------------------------

# Correlation

torgerson_correlation = torgerson(distance_correlation)
df_correlation = data.frame(
  Z1=torgerson_correlation[,1], Z2=torgerson_correlation[,2], 
  Model=Cars93$Model)
ggplot(df_correlation, aes(x=Z1,y=Z2),main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

# 80: --------------------------------------------------------------------------
