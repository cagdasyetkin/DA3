#DA3_Assignment 2
#Business Analytics Part Time
# Gomes, Nuno
# Yetkin, Cagdas

#Task 1 

# Clear the console
cat("\f")

# Clean up the memory
rm(list=ls())

library(haven)
library(ggplot2)
library(data.table)
library(lspline)
library(ggthemes)
library(wbstats)
library(sqldf)

library(arm)
library(readr)
library(lmtest)
library(splines)
library(stargazer)
library(sandwich)

####Importing the data and creating a csv file

#get 	"GDP per capita, PPP (constant 2011 international $)"    (NY.GDP.PCAP.PP.KD)

myWorkingDir <- '/Users/Cagdas/OneDrive/CEU/OneDrive - Central European University/Fall 2017/DA3/group_assignment'
#myWorkingDir <- '/Users/828142'
setwd(myWorkingDir)

dt_gdp<-wb(country = "all", indicator = "NY.GDP.PCAP.PP.KD", startdate= 2014, enddate= 2014,
           lang = c("en"), removeNA = TRUE,
           POSIXct = FALSE, includedec = FALSE)

# get "Life expectancy at birth, total (years)"     (SP.DYN.LE00.IN)

dt_lexp <- wb(country = "all", indicator = "SP.DYN.LE00.IN", startdate= 2014, enddate= 2014,
              lang = c("en"), removeNA = TRUE,
              POSIXct = FALSE, includedec = FALSE)

##write.table("filename.xls", sep="\t", col.names = NA, row.names = TRUE)


#Now we horizontally merge the two datasets. keeping the data when there is a match between both datasets
#We've merged by date and country. Merging by year here might be redundant given that we are only considering 2014 in our analysis, but this way our code is ready in case we want to extende our analysis.

dt <- merge(dt_gdp,dt_lexp,by=c("date","country"))


names(dt)[3]<-"GDP_per_capita"
names(dt)[7]<-"life_expectancy"

dt_csv <- sqldf("select country,GDP_per_capita,life_expectancy from dt")

# save an csv file. 


write.table(dt_csv, file = "da3_a2_task1_dt.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")

dt_csv <- fread("da3_a2_task1_dt.csv")
# Summary statistics GDP Per Capita and Life Expectancy

#Now that we already have the file ready using code, lets start from the beginning, cleaning everything and opening the file recently created using fread


cat("\f")
rm(list=ls())

##Set wd
#setwd("/Users/NMG-I/Desktop/Data Analysis 3/Group Assignment/")

dt <- fread("da3_a2_task1_dt.csv")

#Summary statistics on GDP and Life Expectancy

gdp_stat <- dt[,.(mean_gdp = mean(GDP_per_capita) , sd_gdp = sd(GDP_per_capita),
                  min_gdp = min(GDP_per_capita) , max_gdp = max(GDP_per_capita),
                  p50=quantile(GDP_per_capita,.50), p95=quantile(GDP_per_capita,.95), n=.N )]

gdp_stat


life_exp_stat <-dt[,.(mean_life_exp = mean(life_expectancy) , sd_life_exp = sd(life_expectancy),
                      min_life_exp = min(life_expectancy) , max_life_exp = max(life_expectancy),
                      p50 = quantile(life_expectancy,.50), p95 = quantile(life_expectancy,.95), n=.N )]

life_exp_stat

stargazer(dt, type = "text", title="Descriptive statistics", digits=1)

## histogram of GDP per capita and Life Expectancy
## Not sure if histograms will be useful here to complement our analysis

qplot(dt$GDP_per_capita, geom="histogram", binwidth=1000) + ggtitle('Histogram of GDP per Capita') +
  labs(x="GDP Per Capita",y="Frequency") +
  xlim(0, 150000)+
  theme_economist() + scale_colour_economist()



qplot(dt$life_expectancy, geom="histogram",binwidth=3,xlab="Life Expectancy",ylab="Frequency", main="Histogram of Life Expectancy")+
  xlim(40, 100)+
  theme_economist() + scale_colour_economist()


# LOWESS NONPARAMETRIC REGRESSION: general solution with loess
dt <- dt[,ln_gdp := log(GDP_per_capita)]

ggplot(data = dt, aes(x=ln_gdp, y=life_expectancy)) +
  geom_point(shape=16) +
  ylim(40,100)+
  xlim(0,140000)+
  ggtitle('LOESS') +
  scale_x_continuous(labels = scales::comma) +
  geom_smooth(method="loess",col='darkblue', se=F)+
  labs(x="Log GDP per Capita (USD)",y="Life Expectancy (years)")+
  theme_economist() + scale_colour_economist()

### LINEAR REGRESSIONS
summary(dt)
# lm function
reg_1 <- lm(life_expectancy ~ GDP_per_capita, data=dt)
reg_2 <- lm(life_expectancy ~ ln_gdp, data=dt)
reg_3 <- lm(life_expectancy ~ poly(ln_gdp, 2), data=dt) # Quadratic
reg_4 <- lm(life_expectancy ~ poly(ln_gdp, 3), data=dt) # Cubic

stargazer(reg_1, reg_2, reg_3, reg_4, type = 'text', align = T, out = 'gdp_reg.txt')

#Beta estimates and CI
coeftest(reg_1, vcov=sandwich)
coeftest(reg_2, vcov=sandwich)
coeftest(reg_3, vcov=sandwich)
coeftest(reg_4, vcov=sandwich)

ggplot(data = dt, aes(x=ln_gdp, y=life_expectancy)) +
  geom_point(size=1.5,shape=4) +
  geom_smooth(method="lm", se=F)+ 
  ylim(40,100)+
  ggtitle('Linear Model with Log Gdp') +
  labs(x="Log GDP per Capita (USD)",y="Life Expectancy (years)")+
  theme_economist() + scale_colour_economist()

ggplot(data = dt, aes(x=ln_gdp, y=life_expectancy)) +
  geom_point(size=1.5,shape=4) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1, se=FALSE)+
  ylim(40,100)+
  ggtitle('Quadratic') +
  labs(x="Log GDP per Capita (USD)",y="Life Expectancy (years)")+
  theme_economist() + scale_colour_economist()

ggplot(data = dt, aes(x=ln_gdp, y=life_expectancy)) +
  geom_point(size=1.5,shape=4) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, se=FALSE)+
  ylim(40,100)+
  ggtitle('Cubic') +
  labs(x="Log GDP per Capita (USD)",y="Life Expectancy (years)")+
  theme_economist() + scale_colour_economist()


#### 3

# get the population total
#	Population, total(SP.POP.TOTL)

dt_population<-wb(country = "all", indicator = "SP.POP.TOTL", startdate= 2014, enddate= 2014,
                  lang = c("en"), removeNA = TRUE,
                  POSIXct = FALSE, includedec = FALSE)

#merge population data with the previous dataset 

dt_pop_weight <- merge(dt,dt_population,by=c("country"))

#change the name of the column
names(dt_pop_weight)[6]<-"Population"

#selecting the columns we want to analyse 

dt_pop <- sqldf("select country,GDP_per_capita,life_expectancy,Population from dt_pop_weight")


#using the log gdp per capita
dt_pop$ln_gdp <- log(dt_pop$GDP_per_capita)

reg_weight <- lm(life_expectancy ~ ln_gdp , data=dt_pop, weights = Population)


ggplot (dt_pop, aes (x = ln_gdp, y = life_expectancy, size = Population)) + 
  geom_point(shape = 21, fill = "#003F79", alpha = 0.3, show.legend = FALSE) + 
  scale_size_continuous(range = c(1, 10)) +
  ggtitle ("Weighted Regression- Population") +
  labs(x="Log GDP per capita (USD)", y="Life expectancy (years)") +
  geom_line(data=dt_pop_weight_1,aes(x=lngdppc,y=predict(reg_weight)),colour="blue",size = 1)+
  theme_economist() + scale_colour_economist()


stargazer(reg_weight, type = "text", align = T)

#write the table to csv for moodle submission
write.table(dt_pop, file = "GDP_Pop.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")



