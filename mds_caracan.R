---
title: "Multidimensional Scaling"
author: "Cara Cannella, Alexandra Elias, Alex Kremzier"
date: "November 25, 2018"
output: html_document
---

## Introduction to Multidimensional Scaling

Multidimensional scaling (MDS) is a useful visualization technique that displays information given in a distance matrix.
“In general, the goal of the analysis is to detect meaningful underlying dimensions that allow the researcher to explain 
observed similarities or dissimilarities” (quote). Simply put, objects that have small distances on the graph are more similar 
than objects that have longer distances. There are two approaches to MDS: metric and nonmetric and this tutorial focuses on 
metric MDS. 

## Data Description

The data used in the following example comes from the Cars93 dataset available in the R's MASS package. The observations of
the dataset are 93 randomly selected 1993 passenger car models listed in the Consumer Reports issue and PACE Buying Guide.
Additional information contained in the dataset includes various automobile metrics such as price, horsepower, and luggage 
capacity. For raw data click [here.](http://jse.amstat.org/datasets/93cars.dat.txt?fbclid=IwAR19EJDNQzMMQOBe6ZC4t4li0zx0UpkDBAoSWDNWa3T54JXc-yfpPiPIqsk)


## Implementation {.tabset .tabset-fade}

### R (smacof & data.table)
```{r, message=FALSE}
# Libraries 
library(MASS)
library(tidyverse)
library(ggplot2)
library(data.table)
library(smacof)
```

```{r}
# Get data
Cars93 = as.data.table(MASS::Cars93)

Cars93 = Cars93[!is.na(Rear.seat.room) & !is.na(Luggage.room)]

Cars93_cont_scaled = Cars93[,.(Min.Price, Price, Max.Price,
                               MPG.city, MPG.highway, EngineSize,
                               Horsepower, RPM, Rev.per.mile,
                               Length, Wheelbase, Width,
                               Turn.circle, Rear.seat.room,
                               Luggage.room)] %>%
                      scale()

for (method in c('euclidean', 'manhattan', 'maximum')) {
  assign(paste("distance", method, sep='_'), dist(Cars93_cont_scaled, method = method))
}

distance_correlation = sqrt(2*(1-cor(t(Cars93_cont_scaled))))

# Euclidean Distance
torgerson_euclidean = torgerson(distance_euclidean)
df_euclidean = data.frame(
  Z1=torgerson_euclidean[,1], Z2=torgerson_euclidean[,2], 
  Manufacturer=Cars93$Manufacturer,
  Model=Cars93$Model,
  Type=Cars93$Type,
  DriveTrain=Cars93$DriveTrain,
  Cylinders=Cars93$Cylinders,
  Passengers=Cars93$Passengers,
  Origin=Cars93$Origin)
ggplot(df_euclidean, aes(x=Z1,y=Z2),main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

# Manhattan Distance
torgerson_manhattan = torgerson(distance_manhattan)
df_manhattan = data.frame(
  Z1=torgerson_manhattan[,1], Z2=torgerson_manhattan[,2], 
  Manufacturer=Cars93$Manufacturer,
  Model=Cars93$Model,
  Type=Cars93$Type,
  DriveTrain=Cars93$DriveTrain,
  Cylinders=Cars93$Cylinders,
  Passengers=Cars93$Passengers,
  Origin=Cars93$Origin)
ggplot(df_manhattan, aes(x=Z1,y=Z2),main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

# Maximum Distance 
torgerson_maximum = torgerson(distance_maximum)
df_maximum = data.frame(
  Z1=torgerson_maximum[,1], Z2=torgerson_maximum[,2], 
  Manufacturer=Cars93$Manufacturer,
  Model=Cars93$Model,
  Type=Cars93$Type,
  DriveTrain=Cars93$DriveTrain,
  Cylinders=Cars93$Cylinders,
  Passengers=Cars93$Passengers,
  Origin=Cars93$Origin)
ggplot(df_maximum, aes(x=Z1,y=Z2),main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

# Correlation
torgerson_correlation = torgerson(distance_correlation)
df_correlation = data.frame(
  Z1=torgerson_correlation[,1], Z2=torgerson_correlation[,2], 
  Manufacturer=Cars93$Manufacturer,
  Model=Cars93$Model,
  Type=Cars93$Type,
  DriveTrain=Cars93$DriveTrain,
  Cylinders=Cars93$Cylinders,
  Passengers=Cars93$Passengers,
  Origin=Cars93$Origin)
ggplot(df_correlation, aes(x=Z1,y=Z2),main='Main') + 
  geom_point() + 
  geom_text(aes(label=Model), hjust=-0.2, size=3)

```


### R (Base R & dplyr)
```{r}
# Libraries
library(MASS)
library(tidyverse)
library(dplyr)
library(ggplot2)

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

```

### Stata


## References

## To Do
Collaborate with code
Add text to examples
Make graphs visually appealing
