# CLEAR MEMORY
rm(list=ls())

library(arm)
library(readr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(descr)
library(DataCombine)
library(stargazer)
library(mfx)
library(dummies)
library(data.table)
library(corrplot)

# CKECK  WORKING DIRECTORY
getwd()
setwd('C:/Users/Cagdas/OneDrive/CEU/OneDrive - Central European University/Fall 2017/DA3/ind_assignment')

# LOAD  DATA
share <- read.csv("mortality_oldage_eu.csv", na.strings = ".")
share <- subset(share, age>=50 & age<=80)
share <- subset(share, ever_smoked!="." & eduyears_mod!="." & income10g!=".")
share$deceased <- as.numeric(share$deceased)
share$sports <- as.numeric(share$sports)

#reorder the variables
DT <- share[c("deceased", "female", "age", "eduyears_mod", "income10g", "sports")]
DT <- data.frame(DT)

DT$sports_alot  <- ifelse(DT$sports == 1, 1, 0)
DT$sports_often <- ifelse(DT$sports == 2, 1, 0)
DT$sports_some  <- ifelse(DT$sports == 3, 1, 0)

DT <- DT[complete.cases(DT),]
summary(DT)

# Frequency tables
freq(DT$sports, DT$deceased)
freq(DT$deceased)
str(DT)
#check the correlations
M <- cor(DT)
corrplot(M, method = "circle")

# LPM: linear probability model
lpm1 <- lm(deceased ~ sports_alot + sports_often + sports_some, data=DT)
coeftest(lpm1, vcov=sandwich)


stargazer(lpm1, digits=3, type="text", out="sports_mortality_1.doc")
stargazer(lpm1, type = "text")

lpm2 <- lm(deceased ~  sports_alot + sports_often + sports_some + 
                       female + age + eduyears_mod + income10g, data=DT)

coeftest(lpm2, vcov=sandwich)
stargazer(lpm2, digits=3, type="text", out="sports_mortality_2.doc")
stargazer(lpm2, type = "text")

logitcoeffs1 <- glm(deceased ~ sports_alot + sports_often + sports_some, data=DT, family='binomial')
logitcoeffs2 <- glm(deceased ~ sports_alot + sports_often + sports_some + 
                    female + age + eduyears_mod + income10g, data=DT, family='binomial')

logitmarg1 <- logitmfx(formula = deceased ~ sports_alot + sports_often + sports_some, data=DT, atmean=FALSE)
summary(logitcoeffs1)
print(logitmarg1)

logitmarg2 <- logitmfx(formula = deceased ~ sports_alot + sports_often + sports_some + 
                                 female + age + eduyears_mod + 
                                 income10g, data=DT, atmean=FALSE)
summary(logitcoeffs2)
print(logitmarg2)


