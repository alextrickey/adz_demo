# Data Science Use Cases in Ad Tech
# Problem Set - SOLUTIONS


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

# Why are there sometimes more impressions than clicks?
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

# Demo: Dog food time series
ts_dog <- ts(ad_data[ad_type == "dog_food", rpc], frequency = 24)


#####################################################
# B. Create visualizations of the TS using decomp() #
#####################################################

# Demo: Dog food time series
decomp_dog <- stl(na.interp(ts_dog), s.window="periodic")
plot(decomp_dog)


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

# Demo: Split dog food time series
train_ts_dog <- ts(na.interp(ts_dog[1:train_end]),
                   frequency = 24)
test_ts_dog <- ts(na.interp(ts_dog[test_start:n_hours]), 
                  start = c(test_start/24, 1),
                  frequency = 24)


############################################
# E. Fit an ARIMA Model using auto.arima() #
############################################

# Demo: ARIMA model for dog food time series
fit1 <- auto.arima(train_ts_dog)
accuracy(forecast(fit1, h = 24), test_ts_dog)


######################################
# F. Fit a TBATS Model using tbats() #
#    using a 24 hour seasonal period #
######################################

# Demo: TBATS model for dog food time series
fit2 <- tbats(train_ts_dog, seasonal.periods = 24)
accuracy(forecast(fit2, h=24), test_ts_dog)


###############################################################
# G. Fit a TBATS Model with daily and weekly seasonal periods #
###############################################################

# Demo: Try adding a weekly period the seasonality
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

# Try to repeat this analysis for the cat toys and/or phone service ads




#############
# Problem 3 #
#############

############################
# A. Day1 Model Comparison #
############################

# On day 1 we sent 20% of traffic to the new optimizer.  

# Read in and check the data
day1 <- fread("adz_demo/data/day1.csv")
head(day1)

# Create confidence intervals for the RPS estimates
stats <- day1[, .(m = mean(rps), sd = sd(rps), .N,
                  moe = 1.96*sd(rps)/sqrt(.N)), by = baseline]
stats[, `:=`(lower = m - moe, upper = m + moe)]
print(stats)

# Does it seems safe to send more traffic to the new optimizer?



############################
# B. Day2 Model Comparison #
############################

# On day 2 we sent 50% of traffic to the new optimizer. 

# Repeat the analysis for the second day
day2 <- fread("adz_demo/data/day2.csv")
stats <- day2[, .(m = mean(rps), sd = sd(rps), .N,
                  moe = 1.96*sd(rps)/sqrt(.N)), by = baseline]
stats[, `:=`(lower = m - moe, upper = m + moe)]
stats

# Does it make sense to increase the traffic % again?



############################
# C. Day3 Model Comparison #
############################

# On day 3 we sent 80% of traffic to the new optimizer. 

# Repeat the analysis for the third day
day3 <- fread("adz_demo/data/day3.csv")
head(day3)

stats <- day3[, .(m = mean(rps), sd = sd(rps), .N,
                  moe = 1.96*sd(rps)/sqrt(.N)), by = baseline]
stats[, `:=`(lower = m - moe, upper = m + moe)]
print(stats)


#########################
# D. Combine Comparison #
#########################

# Great things look good! Let's combine the three days, so we can report
# back to the team...

combined <- rbind(day1, day2, day3)
stats <- combined[, .(m = mean(rps), sd = sd(rps), .N,
                      moe = 1.96*sd(rps)/sqrt(.N)), by = baseline]
stats[, `:=`(lower = m - moe, upper = m + moe)]
print(stats)

# Wait... what?! What happened? Why? Explore the data to understand this
# unexpected pattern of results. 

# Visualize RPS
library(ggplot2)
combined[, date := as.Date(date)]
combined[, baseline := as.factor(baseline)]
ggplot(data = combined[, .(mean_rps = mean(rps), .N), by = .(baseline, date)],
       aes(baseline, y = mean_rps, x = date, col = baseline, size = N)) +
  geom_point()
