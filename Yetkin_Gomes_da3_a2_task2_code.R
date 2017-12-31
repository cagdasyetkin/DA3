#DA3_Assignment 2
#Business Analytics Part Time
#Gomes, Nuno
#Yetkin, Cagdas

#Task 2

# Clear the console
cat("\f")

# Clean up the memory
rm(list=ls())

library(haven)
library(data.table)
library(arm)
library(readr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(segmented)
library(stargazer)


#set wd
myWorkingDir <- 'C:/Users/Cagdas/OneDrive/CEU/OneDrive - Central European University/Fall 2017/DA3/group_assignment'
setwd(myWorkingDir)

#open the file
myCity <- 'Milan'
hotels <- fread("hotels_all_nov21.csv")

hotels[city == myCity & stars >= 2, .N, by = accommodation_type]

#Actually we dont have Hostels in Milan just Hotels

dt<-hotels[accommodation_type %in% c("Hotel", "Hostel") & city=='Milan' & stars >= 2,]
dt[, .N, by = accommodation_type]

stargazer(dt, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")

##1. Pick a set of variables. Describe all variables used in the analysis. 
#3 variables to be used: distance, stars, rating. 
which(is.na(dt$rating))
which(is.na(dt$stars))
which(is.na(dt$distance))

#There are only 3 missing values in rating. It is a quick operation to change them.
dt[95]$stars
dt[289]$stars
dt[426]$stars

#Simply change these NA values to average rating for those hotels with the same stars
dt[95]$rating <- dt[stars == 3, mean(rating, na.rm=T)]
dt[289]$rating <- dt[stars == 4, mean(rating, na.rm=T)]
dt[426]$rating <- dt[stars == 3, mean(rating, na.rm=T)]

#check some basic stats
distance_stat <- dt[,.(mean_dist = round(mean(distance),2) , sd_dist = round(sd(distance),2),
                       min_dist = min(distance) , max_dist = max(distance),
                       p50 = quantile(distance,.50), p95 = quantile(distance,.95), n=.N )]


stars_stat <- dt[,.(mean_dist = round(mean(stars),2) , sd_dist = round(sd(stars),2),
                    min_dist = min(stars) , max_dist = max(stars),
                    p50 = quantile(stars,.50), p95 = quantile(stars,.95), n=.N )]

rating_stat <- dt[,.(mean_dist = round(mean(rating),2) , sd_dist = round(sd(rating),2),
                     min_dist = min(rating) , max_dist = max(rating),
                     p50 = quantile(rating,.50), p95 = quantile(rating,.95), n=.N )]

#Take only the hotels until 10 km because after 10 km it is not Milano
dt <- dt[distance <= 10,]

#See the distributions

qplot(dt$distance, geom="histogram", binwidth=1) + ggtitle('Histogram of Distance') +
  labs(x="Distance",y="Hotel Count")

qplot(dt$stars, geom="histogram", binwidth=0.5) + ggtitle('Histogram of Stars') +
  labs(x="Stars",y="Hotel Count")

qplot(dt$rating, geom="histogram", binwidth=0.5) + ggtitle('Histogram of Rating') +
  labs(x="Rating",y="Hotel Count")

#investigate non linearities
#distance 
dt$lnprice <- log(dt$price)

reg_dist     <- lm(lnprice ~ distance, data=dt)
reg_dist_sqr <- lm(lnprice ~ poly(distance, 2), data = dt)
reg_dist_cub <- lm(lnprice ~ poly(distance, 3), data = dt)

stargazer(reg_dist, reg_dist_sqr, reg_dist_cub, type = "text", align = T)

#stars
reg_star     <- lm(lnprice ~ stars, data=dt)
reg_star_sqr <- lm(lnprice ~ poly(stars, 2), data = dt)
reg_star_cub <- lm(lnprice ~ poly(stars, 3), data = dt)

stargazer(reg_star, reg_star_sqr, reg_star_cub, type = "text", align = T)

#rating
reg_rate     <- lm(lnprice ~ rating, data=dt)
reg_rate_sqr <- lm(lnprice ~ poly(rating, 2), data = dt)
reg_rate_cub <- lm(lnprice ~ poly(rating, 3), data = dt)

stargazer(reg_rate, reg_rate_sqr, reg_rate_cub, type = "text", align = T)

#Multiple Regression:

dt$lndistance <- log(dt$distance)

reg_mult_1    <- lm(lnprice ~ distance + stars + rating , data=dt)
coeftest(reg_mult_1, vcov=sandwich)

reg_mult_2    <- lm(lnprice ~ lndistance + stars + rating , data=dt)
coeftest(reg_mult_2, vcov=sandwich)

stargazer(reg_mult_1, reg_mult_2, type = "text", align = T)

# smallest residuals to be checked to find the best deals
dt <- dt[,`:=`(predMult_2 = predict(reg_mult_2), Mult_2_e = resid(reg_mult_2))]

bestdeals <- dt[order(Mult_2_e)][0:5]
write.table(bestdeals, file = "bestdeals.csv",row.names=F, na="",col.names=TRUE, sep=",")


#Pick another city
OtherCity <- "Munich" #why not..
hotels[city == OtherCity & stars >= 2, .N, by = accommodation_type]

OtherDT <- hotels[accommodation_type %in% c("Hotel", "Hostel") & city== OtherCity & stars >= 2,]
OtherDT[, `:=`(lnprice = log(price), lndistance = log(distance))]

stargazer(OtherDT, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")

OtherDT <- OtherDT[complete.cases(OtherDT), ]

reg_Other <- lm(lnprice ~ lndistance + stars + rating , data=OtherDT, na.action=na.omit)
coeftest(reg_Other, vcov=sandwich)

stargazer(reg_Other, type = "text", align = T)

#fairly good result in another city also