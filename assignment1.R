#Load libraries.
library(sf)
library(spData)
library(tidyverse)
install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(spDataLarge)

#Check basic info
names(world)
plot(world)
unique(world$type)

#Tasks:

#1)All countries in Eastern Europe with a population (pop) greater than 5 million
east_europe = world %>% filter(subregion=="Eastern Europe")
east_europe
east_europe = east_europe %>% filter(pop>5000000)
east_europe
plot(east_europe["pop"])

#2)All countries in Africa with a GDP per capita (gdpPercap) of $2000 (more than)
africa = world %>% filter(continent=="Africa")
africa
africa = africa %>% filter(gdpPercap>2000)
plot(africa["gdpPercap"])

#3)All countries in Central America with a life expectancy less (lifeExp) than 65 years old.
cent_america = world %>% filter(subregion=="Central America")
cent_america
cent_america = cent_america %>% filter(lifeExp<65)
cent_america["lifeExp"]
#There are no countries with life expectation below 65 years.

#4) All countries with a political condition (type) of dispute
dispute = world %>% filter(type=="Disputed")
dispute
plot(st_geometry(world %>% filter(subregion=="Western Asia")))
plot(dispute["type"], add=TRUE)
#There is only one country so i decided to show it on subregion.