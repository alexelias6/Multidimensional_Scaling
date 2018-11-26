## 
## Mutidimensional Scaling Tutorial 
## Author: Alexandra Elias, alexeli@umich.edu
##

# 80: -------------------------------------------------------------------------- 

# libraries: -------------------------------------------------------------------

library(MASS)
library(tidyverse)
library(dplyr)
library(ggplot2)

# 80: -------------------------------------------------------------------------- 

# Get data

cars93 = 
  as_tibble(Cars93)

# Clean data

cars93 = filter(cars93,!is.na(Rear.seat.room) & !is.na(Luggage.room))

cars93_scaled = 
  cars93 %>%
  select(Min.Price, Price, Max.Price,
         MPG.city, MPG.highway, EngineSize,
         Horsepower, RPM, Rev.per.mile,
         Length, Wheelbase, Width,
         Turn.circle, Rear.seat.room,
         Luggage.room) %>%
  scale() %>%
  as_tibble()


for (method in c('euclidean', 'manhattan', 'maximum')) {
  assign(paste("distance", method, sep='_'), dist(cars93_scaled, method = method))
}

distance_correlation = sqrt(2*(1-cor(t(cars93_scaled))))

# 80: -------------------------------------------------------------------------- 

# Euclidean Distance

euclidean = cmdscale(distance_euclidean, k=2)
  
euclidean_df = data_frame(
  Z1=euclidean[,1], Z2=euclidean[,2], 
  Manufacturer=cars93$Manufacturer,
  Model=cars93$Model,
  Type=cars93$Type,
  DriveTrain=cars93$DriveTrain,
  Cylinders=cars93$Cylinders,
  Passengers=cars93$Passengers,
  Origin=cars93$Origin)

ggplot(euclidean_df, aes(x=Z1,y=Z2), main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

# 80: --------------------------------------------------------------------------

# Manhattan Distance

manhattan = cmdscale(distance_manhattan, k=2)

manhattan_df = data.frame(
  Z1=manhattan[,1], Z2=manhattan[,2], 
  Manufacturer=cars93$Manufacturer,
  Model=cars93$Model,
  Type=cars93$Type,
  DriveTrain=cars93$DriveTrain,
  Cylinders=cars93$Cylinders,
  Passengers=cars93$Passengers,
  Origin=cars93$Origin)

ggplot(manhattan_df, aes(x=Z1,y=Z2),main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

# 80: --------------------------------------------------------------------------

# Maximum Distance 

maximum = cmdscale(distance_maximum, k=2)

maximum_df = data.frame(
  Z1=maximum[,1], Z2=maximum[,2], 
  Manufacturer=cars93$Manufacturer,
  Model=cars93$Model,
  Type=cars93$Type,
  DriveTrain=cars93$DriveTrain,
  Cylinders=cars93$Cylinders,
  Passengers=cars93$Passengers,
  Origin=cars93$Origin)

ggplot(maximum_df, aes(x=Z1,y=Z2),main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

# 80: --------------------------------------------------------------------------

# Correlation

correlation = cmdscale(distance_correlation, k=2)

correlation_df = data.frame(
  Z1=correlation[,1], Z2=correlation[,2], 
  Manufacturer=cars93$Manufacturer,
  Model=cars93$Model,
  Type=cars93$Type,
  DriveTrain=cars93$DriveTrain,
  Cylinders=cars93$Cylinders,
  Passengers=cars93$Passengers,
  Origin=cars93$Origin)

ggplot(correlation_df, aes(x=Z1,y=Z2),main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

# 80: --------------------------------------------------------------------------