# Data Science Use Cases in Ad Tech
# Problem Set


#############
# Problem 1 #
#############

###################################################################
# A. Load the data from adz_demo/data/hourly_ad_category_data.csv #
###################################################################

# Hint: If using read.csv use the option: colClasses = c("ts" = "POSIXct")



#####################################
# B. View and/or summarize the data #
#####################################




############################################
# C. What problems do you see in the data? #
############################################

# What's missing?


# Is there anything that doesn't make sense?



##################################################################
# D. Try to summarize the data using statistics / visualizations #
##################################################################

# How do RPI and RPC vary with the Ad Type?


# Is there any dependence on time?



#############
# Problem 2 #
#############

#########################################################
# A. Convert rpc's for each ad into time-series objects #
#########################################################
library(forecast) #must be installed outside of R

# Dog food ad


# Cat toys ad


# Phone service ad



#####################################################
# B. Create Visualizations of the TS using decomp() #
#####################################################

# Dog food ad


# Cat toys ad


# Phone service ad



###############################################################
# C. Decide how to split the data into train & test intervals #
###############################################################



############################################
# D. Fit an ARIMA Model using auto.arima() #
############################################





######################################
# E. Fit a TBATS Model using tbats() #
#    using a 24 hour seasonal period #
######################################





###################################
# F. Fit a TBATS Model with daily #
# and weekly seasonal periods     #
###################################

# Hint: use seasonal.periods = c(24, 7*24)



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




# Create confidence intervals for the RPS estimates





# Is it safe to send more traffic to the new algo?



############################
# B. Day2 Model Comparison #
############################

# Repeat the analysis for the second day





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

# Use rbind() to combine the datasets and rerun the analysis



# What happened?! Use R to summarize or visualize what's going on.




