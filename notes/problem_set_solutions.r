# Data Science Use Cases in Ad Tech
# Problem Set - PARTIAL SOLUTIONS


#############
# Problem 1 #
#############

###################################################################
# A. Load the data from adz_demo/data/hourly_ad_category_data.csv #
###################################################################

# Hint: If using read.csv or fread set colClasses = c("ts" = "POSIXct")
library(data.table)
ad_data <- fread("adz_demo/data/hourly_ad_category_data.csv",
                 colClasses = c("ts" = "POSIXct"))

#####################################
# B. View and/or summarize the data #
#####################################

print(ad_data)
ad_data[, lapply(.SD, class)]
summary(ad_data)

############################################
# C. What problems do you see in the data? #
############################################

# Let's check for missing values
ad_data[, lapply(.SD, function(c) sum(is.na(c)))]

# Why are there more missing values for rpi than rpc?
ad_data[imps < clicks, .N]

##################################################################
# D. Try to summarize the data using statistics / visualizations #
##################################################################

# RPI and RPC by ad_type
ad_data[!is.na(total_rev),
        .(rpc = sum(total_rev) / sum(clicks),
          rpi = sum(total_rev) / sum(imps)
        ), by = ad_type]


# Visualize RPC (just show one day)
library(ggplot2)
ggplot(data = ad_data[ts >= "2019-05-13",],
       aes(ts, rpc, col = ad_type)) +
  geom_point() +
  geom_smooth()


#############
# Problem 2 #
#############

#########################################################
# A. Convert rpc's for each ad into time-series objects #
#########################################################
library(forecast)

# Dog food ad
ts_dog <- ts(ad_data[ad_type == "dog_food", rpc], frequency = 24)

# Cat toys ad
ts_cat <- ts(ad_data[ad_type == "cat_toys", rpc], frequency = 24)

# Phone service ad
ts_phone <- ts(ad_data[ad_type == "phone_service", rpc], frequency = 24)


#####################################################
# B. Create visualizations of the TS using decomp() #
#####################################################

# Dog food ad
decomp_dog <- stl(na.interp(ts_dog), s.window="periodic")
plot(decomp_dog)

# Cat food ad
decomp_cat <- stl(na.interp(ts_cat), s.window="periodic")
plot(decomp_cat)

# Phone service ad
decomp_phone <- stl(na.interp(ts_phone), s.window="periodic")
plot(decomp_phone)


###############################################################
# C. Decide how to split the data into train & test intervals #
###############################################################

# How much data is there?
n_hours <- length(ts_dog)
print(n_hours) # 528 hours = 22 days

# Let's set aside the last day for testing
train_end <- n_hours - 24
test_start <- train_end + 1

#####################################################
# D. Impute nulls in train and test sets separately #
#####################################################

# Split dog food time series
train_ts_dog <- ts(na.interp(ts_dog[1:train_end]),
                   frequency = 24)
test_ts_dog <- ts(na.interp(ts_dog[test_start:n_hours]), 
                  start = c(test_start/24, 1),
                  frequency = 24)

############################################
# E. Fit an ARIMA Model using auto.arima() #
############################################

fit1 <- auto.arima(train_ts_dog)
accuracy(forecast(fit1, h = 24), test_ts_dog)


######################################
# F. Fit a TBATS Model using tbats() #
#    using a 24 hour seasonal period #
######################################

fit2 <- tbats(train_ts_dog, seasonal.periods = 24)
accuracy(forecast(fit2, h=24), test_ts_dog)


###################################
# G. Fit a TBATS Model with daily #
# and weekly seasonal periods     #
###################################

# Hint: use seasonal.periods = c(24, 7*24)

fit3 <- tbats(train_ts_dog, seasonal.periods = c(24, 7*24))
accuracy(forecast(fit3, h=24), test_ts_dog)


#####################
# H. Choose a model #
#####################

# Based on the results above which model would you choose? Why?

# Plot the forcast for you selected model
plot(forecast(fit2, h=24))

##############################################
# I. Repeat the analysis for another ad type #
##############################################




#############
# Problem 3 #
#############

############################
# A. Day1 Model Comparison #
############################

# Read in and check the data
day1 <- fread("adz_demo/data/day1.csv")
head(day1)

# Create confidence intervals for the RPS estimates
stats <- day1[, .(m = mean(rps), sd = sd(rps), .N,
                  moe = 1.96*sd(rps)/sqrt(.N)), by = baseline]
stats[, `:=`(lower = m - moe, upper = m + moe)]
stats

# Is it safe to send more traffic to the new algo?



############################
# B. Day2 Model Comparison #
############################

# Repeat the analysis for the second day
day2 <- fread("adz_demo/data/day2.csv")
stats <- day2[, .(m = mean(rps), sd = sd(rps), .N,
                  moe = 1.96*sd(rps)/sqrt(.N)), by = baseline]
stats[, `:=`(lower = m - moe, upper = m + moe)]
stats

# Should we increase the traffic % again?



############################
# C. Day3 Model Comparison #
############################

# Repeat the analysis for the third day

# What is the lift our algo provides over baseline?


#########################
# D. Combine Comparison #
#########################

# Great things look good! Let's combine the three days, so we can report
# back to the team...

combined <- rbind(day1, day2, day3)
stats <- combined[, .(m = mean(rps), sd = sd(rps), .N,
                      moe = 1.96*sd(rps)/sqrt(.N)), by = baseline]
stats[, `:=`(lower = m - moe, upper = m + moe)]
stats

#What happened? Why?

# Visualize RPS
library(ggplot2)
combined[, date := as.Date(date)]
combined[, baseline := as.factor(baseline)]
ggplot(data = combined[, .(mean_rps = mean(rps), .N), by = .(baseline, date)],
       aes(baseline, y = mean_rps, x = date, col = baseline, size = N)) +
  geom_point()

