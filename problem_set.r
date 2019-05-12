# Problem 1
# Which kind of ad should we show?


#############
# Problem 1 #
#############

##############################################################
# A. Load the data from adz/data/hourly_ad_category_data.csv #
##############################################################

# Hint: If using read.csv use the option: colClasses = c("ts" = "POSIXct")
ad_data <- read.csv("adz/data/hourly_ad_category_data.csv",
                    colClasses = c("ts" = "POSIXct"))

#####################################
# B. View and/or summarize the data #
#####################################

head(ad_data)
tail(ad_data)
# if in R Studio: View(ad_data)

summary(ad_data)

############################################
# C. What problems do you see in the data? #
############################################

#Let's check for missing values
apply(X = is.na(ad_data), MARGIN = 2, FUN = sum)

#Why are there more missing values for rpi than rpc?
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
ggplot(data = ad_data[ts >= "2019-05-11",],
       aes(ts, rpc, col = ad_type)) +
  geom_point() +
  geom_smooth()


#############
# Problem 2 #
#############

##################################################################
# A.
##################################################################


