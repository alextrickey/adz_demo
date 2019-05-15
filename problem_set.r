# Data Science Use Cases in Ad Tech
# Problem Set


#############
# Problem 1 #
#############

###################################################################
# A. Load the data from adz_demo/data/hourly_ad_category_data.csv #
###################################################################

# Hint: If using read.csv use the option: colClasses = c("ts" = "POSIXct")
ad_data <- read.csv("adz_demo/data/hourly_ad_category_data.csv",
                    colClasses = c("ts" = "POSIXct"))

#####################################
# B. View and/or summarize the data #
#####################################

head(ad_data)
tail(ad_data)

summary(ad_data)

############################################
# C. What problems do you see in the data? #
############################################

# Let's check for missing values
apply(X = is.na(ad_data), MARGIN = 2, FUN = sum)

# Why are there more missing values for rpi than rpc?
sum(ad_data["imps"] < ad_data["clicks"])


##################################################################
# D. Try to summarize the data using statistics / visualizations #
##################################################################

# RPI and RPC by ad_type

# ... via data.table
library(data.table) #install.packages(data.table)
ad_data <- data.table(ad_data)

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
ts1 <- ts(ad_data[ad_type == "phone_service", rpc], frequency = 24)
ts1 <- na.interp(ts1)

# Cat toys ad


# Phone service ad



#####################################################
# B. Create Visualizations of the TS using decomp() #
#####################################################

# Dog food ad
decomp1 <- stl(ts1, s.window="periodic")
plot(decomp1)

# Cat toys ad


# Phone service ad



###############################################################
# C. Decide how to split the data into train & test intervals #
###############################################################



############################################
# D. Fit an ARIMA Model using auto.arima() #
############################################

fit1 <- auto.arima(ts1[1:503])
accuracy(forecast(fit1, h=24), ts1[504:527])


######################################
# E. Fit a TBATS Model using tbats() #
#    using a 24 hour seasonal period #
######################################

fit2 <- tbats(ts1[1:503], seasonal.periods = 24)
accuracy(forecast(fit2, h=24), ts1[504:527])


######################################
# F. Fit a TBATS Model using tbats() #
#    using a 24 hour seasonal period #
######################################

fit3 <- tbats(ts1[1:503], seasonal.periods = c(24, 7*24))
accuracy(forecast(fit3, h=10), ts1[504:527])


#################
# G. Questions? #
#################

# Based on the results above which model would you choose?


# Our accuracy measures in the test sets here are likely to be better
# than what we will observe in production. Where did we go wrong and
# how can we correct the accuracy estimates?


###########################################
# Plot the forcast for you selected model #
###########################################





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

# Visualize RPC (just show one day)
library(ggplot2)
combined[, date := as.Date(date)]
combined[, baseline := as.factor(baseline)]
ggplot(data = combined[, .(mean_rps = mean(rps), .N), by = .(baseline, date)],
       aes(baseline, y = mean_rps, x = date, col = baseline, size = N)) +
  geom_point()

