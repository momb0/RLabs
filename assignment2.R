#Load libraries
library(sf)
library(spData)
library("ggplot2")
library(tidyverse)
library(spDataLarge)
library(rworldmap)
install.packages('rworldmap')

#Load datasets
df_people = read_csv("russian_passenger_air_service_2.csv")
df_cargo = read_csv("russian_air_service_CARGO_AND_PARCELS.csv")

df_p_fresh = df_people %>%
  filter(`Whole year`>0) 

df_pass_ave = df_p_fresh %>%
  arrange(`Airport name`) %>%
  group_by(`Airport name`) %>%
  summarize(total_flights = sum(`Whole year`), ave_flights = round(mean(`Whole year`),digits=0) , 
            jan=round(mean(January)), feb=round(mean(February)), mar=round(mean(March)), apr=round(mean(April)), 
            may=round(mean(May)), jun=round(mean(June)),jul=round(mean(July)),aug=round(mean(August)),sep=round(mean(September)),
            oct=round(mean(October)),nov=round(mean(November)),dec=round(mean(December)),
            location= first(`Airport coordinates`)) 

patternLeft= "\\b\\d+([\\.,]\\d+)?"
patternRight=",(([^.,]*.[^\\/]\\d+))"

#longitude
df_pass_ave = na.omit(df_pass_ave)
df_pass_ave$long = str_match(df_pass_ave$location, pattern = patternLeft)

#latitude
df_pass_ave$lat = str_match(df_pass_ave$location, pattern = patternRight )
decimalpattern = ", Decimal[\\(][\\']"
df_pass_ave$lat = gsub(decimalpattern, "",df_pass_ave$lat)

df_pass_ave$long <- as.numeric(df_pass_ave$long[1:112])
df_pass_ave$lat <- as.numeric(df_pass_ave$lat[1:112])

#1. Perform a similar process for cargo airport dataset
df_c_fresh= df_cargo %>%
  filter(`Whole year`>0)

df_cargo_ave = df_c_fresh %>%
  arrange(`Airport name`) %>%
  group_by(`Airport name`) %>%
  summarize(total_flights = sum(`Whole year`), ave_flights = round(mean(`Whole year`),digits=0) , 
            jan=round(mean(January)), feb=round(mean(February)), mar=round(mean(March)), apr=round(mean(April)), 
            may=round(mean(May)), jun=round(mean(June)),jul=round(mean(July)),aug=round(mean(August)),sep=round(mean(September)),
            oct=round(mean(October)),nov=round(mean(November)),dec=round(mean(December)),
            location= first(`Airport coordinates`)) 

#longitude
df_cargo_ave = na.omit(df_cargo_ave)
df_cargo_ave$long = str_match(df_cargo_ave$location, pattern = patternLeft)

#latitude
df_cargo_ave$lat = str_match(df_cargo_ave$location, pattern = patternRight )
df_cargo_ave$lat = gsub(decimalpattern, "",df_cargo_ave$lat)

df_cargo_ave$long <- as.numeric(df_cargo_ave$long[1:108])
df_cargo_ave$lat <- as.numeric(df_cargo_ave$lat[1:108])

#2.Geographically plot out all the airports and their air traffic data 
#using the following categories (perform the tasks for both cargo and passenger datasets)

#First, make a template for our maps
our_world <- map_data("world")
Russia <- subset(our_world, our_world$region=="Russia")
my_base <- ggplot() + coord_fixed() + xlab("") + ylab("")
ru_basic <- my_base + geom_polygon(data=Russia, aes(x=long, y=lat, group=group), 
                                   colour="light green", fill="light green")

ru_maper <- 
  ru_basic +
  geom_point(data=df_cargo_ave, 
             aes(x=long, y=lat, size=total_flights), colour="Blue", 
             fill="Yellow",pch=21, alpha=I(0.7)) +
  geom_text(data=df_cargo_ave, aes(long, lat, label = df_cargo_ave$`Airport name`), size=2)

#All airports within the location range of: longitude -> 30 - 50 ; latitude -> 40 - 60

#for passengers
df_pass_ave1 = df_pass_ave %>%
  filter(long > 30 & long < 50)
df_pass_ave1 = df_pass_ave1 %>%
  filter(lat > 40 & lat < 60)

ru_maper <- 
  ru_basic +
  geom_point(data=df_pass_ave1, 
             aes(x=long, y=lat, size=total_flights), colour="Blue", 
             fill="Yellow",pch=21, alpha=I(0.7)) +
  geom_text(data=df_pass_ave1, aes(long, lat, label = df_pass_ave1$`Airport name`), size=2)

#for cargo
df_cargo_ave1 = df_cargo_ave %>%
  filter(long > 30 & long < 50)
df_cargo_ave1 = df_cargo_ave1 %>%
  filter(lat > 40 & lat < 60)

ru_maper <- 
  ru_basic +
  geom_point(data=df_cargo_ave1, 
             aes(x=long, y=lat, size=total_flights), colour="Blue", 
             fill="Yellow",pch=21, alpha=I(0.7)) +
  geom_text(data=df_cargo_ave1, aes(long, lat, label = df_cargo_ave1$`Airport name`), size=2)

#All airports with the lowest average flight details in Winter (December, January & February) 
#i.e. less than 200 flights

#for passengers
df_pass_ave2 = df_pass_ave %>%
  filter((df_pass_ave$dec + df_pass_ave$jan + df_pass_ave$feb) < 200)

ru_maper <- 
  ru_basic +
  geom_point(data=df_pass_ave2, 
             aes(x=long, y=lat, size=total_flights), colour="Blue", 
             fill="Yellow",pch=21, alpha=I(0.7)) +
  geom_text(data=df_pass_ave2, aes(long, lat, label = df_pass_ave2$`Airport name`), size=2)

#for cargo
df_cargo_ave2 = df_cargo_ave %>%
  filter((df_cargo_ave$dec + df_cargo_ave$jan + df_cargo_ave$feb) < 200)

ru_maper <- 
  ru_basic +
  geom_point(data=df_cargo_ave2, 
             aes(x=long, y=lat, size=total_flights), colour="Blue", 
             fill="Yellow",pch=21, alpha=I(0.7)) +
  geom_text(data=df_cargo_ave2, aes(long, lat, label = df_cargo_ave2$`Airport name`), size=2)
#All airports with the highest total average flight details in Summer (June, July & August) 
#i.e. greater than 5000 flights

#for passengers
df_pass_ave3 = df_pass_ave %>%
  filter((df_pass_ave$jun + df_pass_ave$jul + df_pass_ave$aug) > 5000)

ru_maper <- 
  ru_basic +
  geom_point(data=df_pass_ave3, 
             aes(x=long, y=lat, size=total_flights), colour="Blue", 
             fill="Yellow",pch=21, alpha=I(0.7)) +
  geom_text(data=df_pass_ave3, aes(long, lat, label = df_pass_ave3$`Airport name`), size=2)

#for cargo
df_cargo_ave3 = df_cargo_ave %>%
  filter((df_cargo_ave$jun + df_cargo_ave$jul + df_cargo_ave$aug) > 5000)

ru_maper <- 
  ru_basic +
  geom_point(data=df_cargo_ave3, 
             aes(x=long, y=lat, size=total_flights), colour="Blue", 
             fill="Yellow",pch=21, alpha=I(0.7)) +
  geom_text(data=df_cargo_ave3, aes(long, lat, label = df_cargo_ave3$`Airport name`), size=2)
#All airports with the lowest total average flight details in Summer (June, July & August) 
#i.e. less than than 5000 flights

#for passengers
df_pass_ave4 = df_pass_ave %>%
  filter((df_pass_ave$jun + df_pass_ave$jul + df_pass_ave$aug) < 5000)

ru_maper <- 
  ru_basic +
  geom_point(data=df_pass_ave4, 
             aes(x=long, y=lat, size=total_flights), colour="Blue", 
             fill="Yellow",pch=21, alpha=I(0.7)) +
  geom_text(data=df_pass_ave4, aes(long, lat, label = df_pass_ave4$`Airport name`), size=2)

#for cargo
df_cargo_ave4 = df_cargo_ave %>%
  filter((df_cargo_ave$jun + df_cargo_ave$jul + df_cargo_ave$aug) < 5000)

ru_maper <- 
  ru_basic +
  geom_point(data=df_cargo_ave4, 
             aes(x=long, y=lat, size=total_flights), colour="Blue", 
             fill="Yellow",pch=21, alpha=I(0.7)) +
  geom_text(data=df_cargo_ave4, aes(long, lat, label = df_cargo_ave4$`Airport name`), size=2)