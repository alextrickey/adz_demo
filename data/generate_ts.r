# Define Data for Time Series Modeling Example
# Outputs CSV used for Problem 1 (see slides and problem1.r)

library(lubridate)
library(ggplot2)


###############
## Functions ##
###############

#Function to Generate Cyclic Data with Noise
cyclic_data <- function(t, amplitude = 1, ave_value = 0, s_noise = 0, shift = 0) {
  c <- ave_value + amplitude * sin(2 * pi * (t - shift) / 24)
  c + rnorm(n = length(t), mean = 0, sd = s_noise)
}

#Function to Generate Click/Volume-like Data
vol_ts <- function(t, amplitude = 9000, ave_value = 8000, s_noise = 2000, shift = 7) {
  r <- cyclic_data(t, amplitude, ave_value, s_noise, shift)
  r <- round(r, digits = 0)
  pmax(r, 0)
}

#Function to Generate Revenue Data
rev_ts <- function(t, amplitude = 0.1, ave_value = 0.15, s_noise = 0.05, shift = 7) {
  r <- cyclic_data(t, amplitude, ave_value, s_noise, shift)
  pmax(r, 0)
}

#Make all data for an Ad
make_ad_data <- function(t, rpc, ad_type) {

  #Create an ad_type vector
  ad_type = rep(ad_type, length(t))

  #Timestamps
  ts = round_date(now(), unit = "hour") + hours(t - max(t))

  #Get clicks
  clicks <- vol_ts(t)
  rpc <- ifelse(clicks > 0, rpc, NA)
  total_rev <- round(rpc * clicks, digits = 2)

  #Generate some garbage impression data
  imps <- 10*vol_ts(t, s_noise = 10000) #garbage
  rpi <- ifelse(imps > 0, total_rev / imps, NA)

  #Return all
  data.frame(ad_type, ts, t, clicks, imps, rpc, rpi, total_rev)
}


####################
## Create Ad Data ##
####################
set.seed(876)

#Define Timeseries
ndays = 22
t = 1:(24*ndays)

#Ad Category 1
rpc <- rev_ts(t)
ad1 <- make_ad_data(t, rpc, ad_type = "dog_food")

#Ad Category 2
rpc <- rev_ts(t, amplitude = 0.09, shift = 5)
ad2 <- make_ad_data(t, rpc, ad_type = "cat_toys")

#Ad Category 3
rpc <- rev_ts(t, amplitude = 0.01, ave_value = 0.15,  s_noise = 0.02)
ad3 <- make_ad_data(t, rpc, ad_type = "phone_service")

#Combine
ts_data <- rbind(ad1, ad2, ad3)

###################
## Write to File ##
###################

#Write TS Data to File
write.csv(x = ts_data,
          file = "adz/data/hourly_ad_category_data.csv",
          row.names = FALSE)


###########################
# Create A/B Testing Data #
###########################
set.seed(995)

baseline <- rbinom(n = 11080, size = 1, prob = 0.8)
day1 <- data.table(baseline)
day1[baseline == 0, rps := rnorm(n = .N, mean = 0.19, sd = 0.05)]
day1[baseline == 1, rps := rnorm(n = .N, mean = 0.18, sd = 0.05)]
day1[, date := "2019-05-13"]

baseline <- rbinom(n = 9727, size = 1, prob = 0.5)
day2 <- data.table(baseline)
day2[baseline == 0, rps := rnorm(n = .N, mean = 0.17, sd = 0.05)]
day2[baseline == 1, rps := rnorm(n = .N, mean = 0.15, sd = 0.05)]
day2[, date := "2019-05-14"]

baseline <- rbinom(n = 12082, size = 1, prob = 0.2)
day3 <- data.table(baseline)
day3[baseline == 0, rps := rnorm(n = .N, mean = 0.15, sd = 0.05)]
day3[baseline == 1, rps := rnorm(n = .N, mean = 0.13, sd = 0.05)]
day3[, date := "2019-05-15"]


###################
## Write to File ##
###################

#Write A/B Data to File
write.csv(x = day1,
          file = "adz/data/day1.csv",
          row.names = FALSE)
write.csv(x = day2,
          file = "adz/data/day2.csv",
          row.names = FALSE)
write.csv(x = day3,
          file = "adz/data/day3.csv",
          row.names = FALSE)

