install.packages("devtools")
devtools::install_github("business-science/anomalize")
devtools::install_github("amrrs/coindeskr")
install.packages("tidyverse")
install.packages("backports")
install.packages("recipes")
install.packages("forecast")
install.packages("xts")

library(anomalize) #tidy anomaly detection
library(tidyverse)
library(coindeskr)
library(dplyr)
library(tibble)
library(tibbletime)
library(recipes)
library(xts)
library(forecast)

df = readr::read_csv('C:/Users/George/Desktop/R/County_time_series.csv')

# 1. Group by Region Name (RegionName) - Select any 2 Regions of your choice.Find the anomalies and plot
# Some information in this dataset are not available, so some graphics will be empty
df_1051 <- df %>% filter(RegionName == "1051")
df_1113 <- df %>% filter(RegionName == "1113")

# a. Anomalies in MedianListingPrice_AllHomes Where MedianListingPricePerSqft_AllHomes > 65
df_1051_1 = df_1051 %>% 
  filter(MedianListingPricePerSqft_AllHomes > 65) %>%
  select(Date, MedianListingPrice_AllHomes)

df_1051_1 = df_1051_1 %>% 
  mutate(date = as.Date(df_1051_1$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1051_1 %>% 
  anomalize::time_decompose(ZHVI_5BedroomOrMore) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "Anomalies in MedianListingPrice_AllHomes Where MedianListingPricePerSqft_AllHomes > 65")
#############################################################
df_1113_1 = df_1113 %>% 
  filter(MedianListingPricePerSqft_AllHomes > 65) %>%
  select(Date, MedianListingPrice_AllHomes)

df_1113_1 = df_1113_1 %>% 
  mutate(date = as.Date(df_1113_1$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1113_1 %>% 
  anomalize::time_decompose(ZHVI_5BedroomOrMore) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "Anomalies in MedianListingPrice_AllHomes Where MedianListingPricePerSqft_AllHomes > 65")

# b. Anomalies in MedianListingPrice_AllHomes Where MedianListingPricePerSqft_AllHomes < 65
df_1051_1 = df_1051 %>% 
  filter(MedianListingPricePerSqft_AllHomes < 65) %>%
  select(Date, MedianListingPrice_AllHomes)

df_1051_1 = df_1051_1 %>% 
  mutate(date = as.Date(df_1051_1$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1051_1 %>% 
  anomalize::time_decompose(MedianListingPrice_AllHomesv) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "Anomalies in MedianListingPrice_AllHomes Where MedianListingPricePerSqft_AllHomes < 65")
#############################################################
df_1113_1 = df_1113 %>% 
  filter(MedianListingPricePerSqft_AllHomes < 65) %>%
  select(Date, MedianListingPrice_AllHomes)

df_1113_1 = df_1113_1 %>% 
  mutate(date = as.Date(df_1113_1$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1113_1 %>% 
  anomalize::time_decompose(MedianListingPrice_AllHomes) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "Anomalies in MedianListingPrice_AllHomes Where MedianListingPricePerSqft_AllHomes < 65")

# 2. General anomaly detection in prices of Homes with more than 5 rooms ZHVI_5BedroomOrMore
df_1051_2 = df_1051 %>% 
  filter(!(is.na(ZHVI_5BedroomOrMore))) %>%
  select(Date, ZHVI_5BedroomOrMore)

df_1051_2 = df_1051_2 %>% 
  mutate(date = as.Date(df_1051_2$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1051_2 %>% 
  anomalize::time_decompose(ZHVI_5BedroomOrMore) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in prices of Homes with more than 5 rooms")
#############################################################
df_1113_2 = df_1113 %>% 
  filter(!(is.na(ZHVI_5BedroomOrMore))) %>%
  select(Date, ZHVI_5BedroomOrMore)

df_1113_2 = df_1113_2 %>% 
  mutate(date = as.Date(df_1113_2$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1113_2 %>% 
  anomalize::time_decompose(ZHVI_5BedroomOrMore) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in prices of Homes with more than 5 rooms")

# 3. General anomaly detection in prices of low cost homes ZHVI_BottomTier
df_1051_3 = df_1051 %>% 
  filter(!(is.na(ZHVI_BottomTier))) %>%
  select(Date, ZHVI_BottomTier)

df_1051_3 = df_1051_3 %>% 
  mutate(date = as.Date(df_1051_3$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1051_3 %>% 
  anomalize::time_decompose(ZHVI_BottomTier) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in prices of low cost homes")
#############################################################
df_1113_3 = df_1113 %>% 
  filter(!(is.na(ZHVI_BottomTier))) %>%
  select(Date, ZHVI_BottomTier)

df_1113_3 = df_1113_3 %>% 
  mutate(date = as.Date(df_1113_3$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1113_3 %>% 
  anomalize::time_decompose(ZHVI_BottomTier) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in prices of low cost homes")

# 4. General anomaly detection in prices of 1-bedroom ZHVI_1bedroom apartments.
df_1051_41 = df_1051 %>% 
  filter(!(is.na(ZHVI_1bedroom))) %>%
  select(Date, ZHVI_1bedroom)

df_1051_41 = df_1051_41 %>% 
  mutate(date = as.Date(df_1051_41$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1051_41 %>% 
  anomalize::time_decompose(ZHVI_1bedroom) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in prices of 1-bedroom ZHVI_1bedroom apartments")
#############################################################
df_1113_41 = df_1113 %>% 
  filter(!(is.na(ZHVI_1bedroom))) %>%
  select(Date, ZHVI_1bedroom)

df_1113_41 = df_1113_41 %>% 
  mutate(date = as.Date(df_1113_41$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1113_41 %>% 
  anomalize::time_decompose(ZHVI_1bedroom) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in prices of 1-bedroom ZHVI_1bedroom apartments")

# General anomaly detection in prices of 2-bedroom ZHVI_2bedroom apartments.
df_1051_42 = df_1051 %>% 
  filter(!(is.na(ZHVI_2bedroom))) %>%
  select(Date, ZHVI_2bedroom)

df_1051_42 = df_1051_42 %>% 
  mutate(date = as.Date(df_1051_42$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1051_42 %>% 
  anomalize::time_decompose(ZHVI_2bedroom) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in prices of 2-bedroom ZHVI_2bedroom apartments")
#############################################################
df_1113_42 = df_1113 %>% 
  filter(!(is.na(ZHVI_2bedroom))) %>%
  select(Date, ZHVI_2bedroom)

df_1113_42 = df_1113_42 %>% 
  mutate(date = as.Date(df_1113_42$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1113_42 %>% 
  anomalize::time_decompose(ZHVI_2bedroom) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in prices of 2-bedroom ZHVI_2bedroom apartments")

# General anomaly detection in prices of 3-bedroom ZHVI_3bedroom apartments.
df_1051_43 = df_1051 %>% 
  filter(!(is.na(ZHVI_3bedroom))) %>%
  select(Date, ZHVI_3bedroom)

df_1051_43 = df_1051_43 %>% 
  mutate(date = as.Date(df_1051_43$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1051_43 %>% 
  anomalize::time_decompose(ZHVI_3bedroom) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in prices of 3-bedroom ZHVI_3bedroom apartments")
#############################################################
df_1113_43 = df_1113 %>% 
  filter(!(is.na(ZHVI_3bedroom))) %>%
  select(Date, ZHVI_3bedroom)

df_1113_43 = df_1113_43 %>% 
  mutate(date = as.Date(df_1113_43$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1113_43 %>% 
  anomalize::time_decompose(ZHVI_3bedroom) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in prices of 3-bedroom ZHVI_3bedroom apartments")

# 5. General anomaly detection in Home Prices Sale_Prices
df_1051_5 = df_1051 %>% 
  filter(!(is.na(Sale_Prices))) %>%
  select(Date, Sale_Prices)

df_1051_5 = df_1051_5 %>% 
  mutate(date = as.Date(df_1051_5$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1051_5 %>% 
  anomalize::time_decompose(Sale_Prices) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in Home Prices")
#############################################################
df_1113_5 = df_1113 %>% 
  filter(!(is.na(Sale_Prices))) %>%
  select(Date, Sale_Prices)

df_1113_5 = df_1113_5 %>% 
  mutate(date = as.Date(df_1113_5$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1113_5 %>% 
  anomalize::time_decompose(Sale_Prices) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in Home Prices")

# 6. General anomaly detection in Homes with decreasing Value PctOfHomesDecreasingInValues_AllHomes
df_1051_6 = df_1051 %>% 
  filter(!(is.na(PctOfHomesDecreasingInValues_AllHomes))) %>%
  select(Date, PctOfHomesDecreasingInValues_AllHomes)

df_1051_6 = df_1051_6 %>% 
  mutate(date = as.Date(df_1051_6$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1051_6 %>% 
  anomalize::time_decompose(PctOfHomesDecreasingInValues_AllHomes) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in Homes with decreasing Value")
#############################################################
df_1113_6 = df_1113 %>% 
  filter(!(is.na(PctOfHomesDecreasingInValues_AllHomes))) %>%
  select(Date, PctOfHomesDecreasingInValues_AllHomes)

df_1113_6 = df_1113_6 %>% 
  mutate(date = as.Date(df_1113_6$Date)) %>%
  select(-one_of('Date')) %>%
  as_tbl_time(index = date) %>%
  as_period("monthly")

df_1113_6 %>% 
  anomalize::time_decompose(PctOfHomesDecreasingInValues_AllHomes) %>%
  anomalize::anomalize(remainder) %>%
  anomalize::time_recompose() %>%
  anomalize::plot_anomaly_decomposition() +
  labs(title = "General anomaly detection in Homes with decreasing Value")