rm(list=ls())

library(dplyr)
library(ggplot2)
library(scales)
library(sandwich)
library(stargazer)
library(data.table)
library(corrplot) # cpr matrix
library(lmtest)
library(descr) #freq function
library(mfx) #logit marginal
library(ggthemes) #plot themes

getwd()

#setwd("/Users/NMG-I/Desktop/Data Analysis 3/Group Assignment 2/")

setwd("C:/Users/Cagdas/OneDrive/CEU/OneDrive - Central European University/Fall 2017/DA3/group_assignment_2")



dbase <- fread("bisnode_all.csv")

dt <- dbase[dbase$sales > 100000 & dbase$sales < 100000000,] # To get rid of small and huge companies. We are doing research on small and midium size corporate
dt <- dt[dt$ind != '',]

dt$ind <- ifelse(dt$ind == 'C Manuf_equipment' | dt$ind == 'C Manuf_auto', 'Manufacturing', 'hotel and restaurant')



dt<-dplyr::filter(dt,  !is.na(birth_year))
dt$birth_year<-trunc(dt$birth_year)
dt <- dt[dt$inoffice_days > 170,]
dt$inoffice_days<-trunc(dt$inoffice_days)
dt <- dt[dt$balsheet_length > 272,]
dt$CEO_age <- as.numeric(2016-dt$birth_year) #lets have an actual continuous age variable
dt <- dt[dt$CEO_age > 20, ] #we dont need the children Ceo



#dummy variable for young CEO

dt$young <- ifelse(dt$CEO_age < 40, 1, 0)

#dt <- dt[dt$ceo_count < 2,] # this is to cross check firms with 1 CEO and more CEOs


ggplot(dt[dt$year==2014,], aes(ind, mean(sales, na.rm= T)/1000000)) +geom_bar(stat= 'identity') + 
  labs(x="Industry",y="Average Sales (Mil EUR)",title="Average Sales per Industry")

ggplot(dt,aes(ind,mean(sales)/1000000)) + geom_col()
dt[ ,mean(dt$sales,na.rm=T), by=ind]


dt <- dt[dt$year==2014 | dt$year==2015,]
dt <- data.table(dt)
dt  <- dt[dt$comp_id %in% dt$comp_id[duplicated(dt$comp_id)],]
dt[, .N, by=year]


dt[, c("COGS", "finished_prod", "net_dom_sales", "net_exp_sales", "wages", "exit_year", "labor_avg"):=NULL]


dt$firmAge <- dt$year - dt$founded_year


dt$ind2 <- as.factor(dt$ind2)
dt$young <- as.factor(dt$young)
dt$gender <- as.factor(dt$gender)
dt$nace_main <- as.factor(dt$nace_main)


dt$female <- round(dt$female, 2)

dt[, .N, by=ind]

dt  <- dt[dt$comp_id %in% dt$comp_id[duplicated(dt$comp_id)],]

table(dt$balsheet_notfullyear)

table(dt$young)

freq(dt$young, main= 'Number of companies by age category', 
     xlab='young')

freq(dt$ind, main= 'Number of companies by Industry', 
     xlab='Industry Code')

freq(dt$region_m, main= 'Number of companies by Region', 
     xlab='Region')

freq(dt$origin, main= 'Number of companies by Origin', 
     xlab='Origin')

freq(dt$foreign)


freq(dt$ceo_count, main= 'Number of CEOs in company', 
     xlab='Number of CEOs')

stargazer(dt, type = "html", out = "raw_data_basics.doc")

summary(dt$ind2) #some industry types dominated this sample

str(dt)

ggplot(dt, aes(firmAge)) + geom_histogram() + labs(x= 'Firm Age', y= 'Number of Firms',title= 'Histogram of firm age')

hist(log(dt$sales))

hist(dt$sales)

table(dt$gender)



sapply(dt, is.factor) #gender, nace_main, ind2, ind, urban_m, region_m,young, begin, end



corrMatrix <- subset(dt, select =  -c(gender, nace_main, ind2, ind, urban_m, region_m,young, begin, end,
                                      ceo_count,birth_year,founded_date,
                                      exit_date,comp_id,D,balsheet_flag,balsheet_notfullyear,year,foreign,origin))

corrMatrix$expenses <- corrMatrix$extra_exp + corrMatrix$personnel_exp + corrMatrix$material_exp

corrMatrix2 <- subset(corrMatrix, select = c(profit_loss_year, curr_liab, liq_assets, expenses, CEO_age))

corrMatrix <- data.table(corrMatrix)
corrMatrix <- corrMatrix[complete.cases(corrMatrix),]

#check the correlations

M <- cor(corrMatrix)
corrplot(M, method = "circle")
corrplot(M, method = "square")


corrMatrix2 <- data.table(corrMatrix2)
corrMatrix2 <- corrMatrix2[complete.cases(corrMatrix2),]

M2 <- cor(corrMatrix2)
corrplot(M2, method = "circle")
corrplot(M2, method = "square")
#corrplot(M, method = "number") # Display the correlation coefficient

summary(dt$material_exp)
summary(dt$personnel_exp)
summary(dt$profit_loss_year)
summary(dt$sales)


dt <- dt[!is.na(profit_loss_year),]


dt[, score := inc_bef_tax / (curr_assets + fixed_assets - curr_liab)] 
dt <- dt[!is.na(score),]



summary(dt$score)
ggplot(dt, aes(log(score))) + geom_histogram() + labs(x= 'Score', y= 'Number of Firms',title= 'Histogram of Score')


boxplot(dt$score)
dt  <- dt[dt$comp_id %in% dt$comp_id[duplicated(dt$comp_id)],]

dt <- data.table(dt)
dt[, .N, by=year]



###########CREATION OF MERGED 2014 2015#############

#make 2014
dt_2014 <- dt[dt$year == '2014',c('comp_id', 'sales', 'score', 'young', 'region_m', 'CEO_age', 'firmAge', 'ind', 'ind2')]

#make 2015
dt_2015 <- dt[dt$year == '2015',c('comp_id', 'sales', 'score', 'young', 'region_m', 'CEO_age', 'firmAge', 'ind', 'ind2')]

#join them by company id
panel <- merge(dt_2014, dt_2015, by = "comp_id", all = F)

###############SALES DIFF ANALYSIS STARTS##################

#add a column for % change in sales

panel$salesdiff <- (panel$sales.y - panel$sales.x) / panel$sales.x
summary(panel$salesdiff)

panel <- panel[which(panel$salesdiff <= quantile(panel$salesdiff, prob = 0.99)),]

panel[, mean(sales.y), by = ind.y]

ggplot(panel[, mean(sales.y)/1000000, by = ind.y], aes(ind.y, V1)) + geom_bar(stat = 'identity') + labs(x= 'Industry', y= 'Sales (Mil EUR)',title= 'Sales by Industry 2015')
ggplot(panel[, mean(sales.x)/1000000, by = ind.y], aes(ind.y, V1)) + geom_bar(stat = 'identity') + labs(x= 'Industry', y= 'Sales (Mil EUR)',title= 'Sales by Industry 2014')




ggplot(panel, aes(CEO_age.x, salesdiff, color = ind.x)) + geom_point()
ggplot(panel, aes(CEO_age.y, salesdiff, color = ind.y)) + geom_point()


ggplot(panel, aes(firmAge.y, salesdiff, color = ind.y)) + 
  geom_point(position = 'jitter') + 
  facet_wrap(~young.y) + geom_smooth(method = 'lm', se = F) + labs(x= 'Firm Age', y='Sales % change', title = ' Sales % change according to Firm age') #+ theme_economist() + scale_colour_economist()

hist(panel$salesdiff)

extreme_high_salesdiff <- panel[which(panel$salesdiff >= quantile(panel$salesdiff, prob = 0.99)),]
str(extreme_high_salesdiff)
xtrem_high <- subset(extreme_high_salesdiff, select = c(comp_id, young.y, salesdiff))
freq(xtrem_high$young.y)


extreme_low_salesdiff <- panel[which(panel$salesdiff <= quantile(panel$salesdiff, prob = 0.01)),]
str(extreme_low_salesdiff)
xtrem_low <- subset(extreme_low_salesdiff, select = c(comp_id, young.y, salesdiff))
freq(xtrem_low$young.y)

#ggplot(panel, aes(firmAge.x, salesdiff, color = ind.x)) + 

#  geom_point(position = 'jitter', aes(colour = cut(salesdiff, c(-Inf, 0, Inf)))) + 

#  scale_color_manual(name = 'qsec', values = c("(-Inf, 0]" = 'red')) +

#  facet_wrap(~young.x) + geom_smooth(method = 'lm', se = F) + theme_economist() + scale_colour_economist()



#ggplot(panel, aes(firmAge.x, salesdiff, color = ind.x)) + 

#  geom_point(position = 'jitter', aes(colour = cut(salesdiff, c(-Inf, 0, Inf)))) + 

#  scale_color_manual(name = 'salesdiff', values = c("(-Inf, 0]" = 'red')) +

#  facet_wrap(~young.x) + geom_smooth(method = 'lm', se = F) + theme_economist() + scale_colour_economist()



#this one is no good

#ggplot(panel, aes(young.x, salesdiff, color = ind.x)) + 

#  geom_point(position = 'jitter') + 

#  facet_wrap(~region_m.x) + geom_smooth(method = 'lm', se = F) + theme_economist() + scale_colour_economist()



#standardize

#panel$salesdiff <- (panel$salesdiff - min(panel$salesdiff))/(max(panel$salesdiff) - min(panel$salesdiff))

#summary(panel$salesdiff)



panel$success <- ifelse(panel$salesdiff >= median(panel$salesdiff), 1, 0)



cor(panel$salesdiff, panel$CEO_age.y)
cor(panel$score.y, panel$CEO_age.y)


panel[, .N, by=ind.y] #C Manuf_equipment, I hotel and restaurant, C Manuf_auto


stargazer(panel, type="html",out="stargazer1.doc")


#############SCORE DIFF ANALYSIS STARTS##################

panel$scorediff <- (panel$score.y - panel$score.x) / (panel$score.x+0.0000001) 

summary(panel$scorediff)

###RUN regression (create column with scores and run regression on age)
panel <- panel[which(panel$scorediff <= quantile(panel$scorediff, prob = 0.99)),]


stargazer(panel, type = "text")


panel$scorediff <- (panel$scoresdiff - min(panel$scorediff))/(max(panel$scorediff) - min(panel$scorediff))
summary(panel$scorediff)  


d <- density(panel$scorediff) # returns the density data 
plot(d)
d2 <- density(panel$salesdiff)
plot(d2)


#salesdiff models
lm1 <- lm(salesdiff ~ young.y, data=panel)
coeftest(lm1, vcov=sandwich)
summary(lm1)

lm2 <- lm(salesdiff ~ young.y + firmAge.y, data=panel)
coeftest(lm2, vcov=sandwich)
summary(lm2)

lm3 <- lm(salesdiff ~ young.y + firmAge.y + ind.y, data=panel)
coeftest(lm3, vcov=sandwich)
summary(lm3)

lmlog <- lm((log(sales.y)-log(sales.x)) ~ young.y, data=panel)
coeftest(lmlog, vcov=sandwich)
summary(lmlog)

stargazer(lm1, lm2, lm3, lmlog, type = 'html', out = 'lm_compare_1.html')

lm4 <- lm(salesdiff ~ firmAge.y, data=panel)
coeftest(lm4, vcov=sandwich)
summary(lm4)


# success if salesdiff higher than average salesdiff
lm9 <- lm(success ~ young.y, data=panel)
coeftest(lm9, vcov=sandwich)
lm10 <- lm(success ~ young.y + firmAge.y, data=panel)
coeftest(lm10, vcov=sandwich)
lm11 <- lm(success ~ young.y + firmAge.y + ind.y, data=panel)
coeftest(lm11, vcov=sandwich)

stargazer(lm9, lm10, lm11, type = 'html', out = 'lm_compare_2.html')

panel <- panel[!is.na(scorediff),]


logitmarg1 <- logitmfx(formula = success ~ CEO_age.y, data=panel, atmean=FALSE)
print(logitmarg1)

#Score models
lm6 <- lm(scorediff ~ young.x, data=panel)
coeftest(lm6, vcov=sandwich)
lm7 <- lm(scorediff ~ young.x + firmAge.x, data=panel)
coeftest(lm7, vcov=sandwich)
lm8 <- lm(scorediff ~ young.x + firmAge.x + ind.x, data=panel)
coeftest(lm8, vcov=sandwich)

stargazer(lm6, lm7, lm8, type = 'html', out = 'lm_compare_3.doc')

table(panel$scorediff<0, panel$young.y)
CrossTable(panel$salesdiff < median(panel$salesdiff), panel$young.y)

lm12 <- lm(salesdiff ~ firmAge.y, data=panel)
coeftest(lm12, vcov=sandwich)
summary(lm12)

ggplot(panel, aes(CEO_age.y, firmAge.y)) + geom_point(position = 'jitter')
#cor(panel$firmAge.y, panel$CEO_age.y)

#what is the %change in sales where the ceo s are old

panel <- data.table(panel)

panel[young.y == 0, mean(salesdiff)]
panel[young.y == 1, mean(salesdiff)]

panel[young.y == 0, mean(scorediff)]
panel[young.y == 1, mean(scorediff)]





#There are 2 companies who jumped from 100 000 sales in 2014 to zillions of dollars sale
#They are also visible in this geom_point plot

ggplot(panel, aes(CEO_age.y, salesdiff)) + geom_point()
ggplot(panel, aes(log(salesdiff))) + geom_histogram()
ggplot(panel, aes(log(sales.x))) + geom_histogram()
ggplot(panel, aes(log(sales.y))) + geom_histogram()


#CEO age distribution in different sectors

ggplot(panel, aes(CEO_age.y)) + geom_histogram(binwidth = 5) + facet_wrap(~ind.y) + labs(x= 'CEO age', y= 'Number of companies',title= 'Histogram Ages per Industry')
ggplot(panel, aes(CEO_age.y, salesdiff, color=ind.y)) + geom_bar(stat = 'identity') + labs(x= 'CEO age', y= 'Sales variation',title= 'Sales variation per industry by age category')

summary(panel$salesdiff)