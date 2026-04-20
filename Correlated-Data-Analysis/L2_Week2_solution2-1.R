#Solution to Exercise 2.2
# See eNote-1 p 20 and eNote13 p 8-9
#-----
#Read the data 
#0. Make an exploratory analysis, e.g. tables and plots

#c) Analyse the effect of the factors on the maillard reaction products. 
# Present your final model including an ANOVA table

#d) Present parameter estimates and post-hoc results (expected means, i.e., LS-means).
#Remember to include confidence intervals to quantify the uncertainty.
#--------
library(lmerTest)

remove(list=ls())
getwd()
#setwd("~/Library/CloudStorage/Dropbox/DTU/DTU_2024/02429_LinearMixedModels/R-code")

# Observe the description of the original experiment given in eNote-13.
# The data corresponds to a repeated measurements analysis/experiment, where
# the 27 samples were stored and measured after 4, 6 and 8 weeks. See data set:

#milk_rep <- read.csv("https://www2.compute.dtu.dk/courses/02429/Data/datafiles/milk.txt")

# The description of the Exercise 2 Maillard reaction in milk powder – temporal version,
# in eNote-2 p 19-20, indicates to assume that the measurements from the three storage times 
# were from different samples (such that there were 81 samples in total), and to use
# dataset in milk2.txt:

milk <- read.csv("https://www2.compute.dtu.dk/courses/02429/Data/datafiles/milk2.txt")


# The sugested analysis for Maillard reaction in milk powder, version II,in eNote13 p8 is:

# "Randomized block (fixed two-way treatment with interaction, random block) for each
# of the six variables. Repeated measures analysis for each set of three time repeated mea-
# surements."

#We are not yet making an analysis for repeated measurements.
#So we shall assume that the the measurements from the three storage times 
# were from different samples, 97 different samples instead of 27 measured three times.

#a)storage can be taken as a fixed effect factor, assuming that interest lies in those
#specific storage levels: 4, 6 and 8.

#b) storage can be considered as a random effect factor, assuming that 
# inference is expected for other time periods of storage.

# Assumming that one is interested in taking storage as a fixed factor.


# 0. Prepare the data for analysis

milk$storage<-factor(milk$storage)
milk$temp<-factor(milk$temp)
milk$water<-factor(milk$water)
milk$rep<-factor(milk$rep)
str(milk)

#I I. Explore the dataset.
summary(milk)
# Are the factors balanced?
table(milk$water)
table(milk$temp) #non balanced
table(milk$rep)
table(milk$storage)

ftable(milk$water,milk$temp,milk$rep,milk$storage)

# Thre are three crossed factors: 3 x 4 x 3 x 3=108 cells
# of which 27 are empty.


#Boxplots and Stripcharts for the groups derived from two variables
par(mai=c(.75,.75,.5,.75))
par(mfrow=c(2,2))
boxplot(maillard ~ water, data = milk, xlab='water',
        ylab='maillard reaction', las=1, col=2:10)
boxplot(maillard ~ temp, data = milk, xlab='temperature',
        ylab='maillard reaction', las=1, col=2:10)
boxplot(maillard ~ storage, data = milk, xlab='storage',
        ylab='maillard reaction', las=1, col=2:10)
boxplot(maillard ~ water:rep, data = milk, xlab='repetition',
        ylab='maillard reaction', las=1, col=2:10)

par(mai=c(.75,.75,.5,.5))
par(mfrow=c(1,3))
plot(milk$maillard, milk$storage)
plot(milk$maillard, milk$temp)
plot(milk$maillard, milk$rep)

par(mai=c(1,1,1,1))
par(mfrow=c(1,1))

#c) Analyse the effect of the factors on the maillard reaction products. 
# Present your final model including an ANOVA table


#We start with the additive mode123d effects and
# one single random effect.
m1<-lmer(maillard~ 1 + storage + temp + water + (1|rep), REML=T,data=milk)
drop1(m1)
summary(m1)
confint(m1)

#Add interactions, one at a time
m2<-lmer(maillard~ 1 + storage + temp + water + temp:water + (1|rep),data=milk)
summary(m2)
drop1(m2)
xtabs(~ temp + water, data=milk)
#There are no observations for (temp=110, water=2) and for (temp=140, water=1)

m3<-lmer(maillard~ 1 + storage + temp + water + temp:storage + (1|rep), REML=T,data=milk)
summary(m3)
drop1(m3)
xtabs(~ temp + storage, data=milk)

m4<-lmer(maillard~ 1 + storage + temp + water + water:storage + (1|rep), REML=T,data=milk)
summary(m4)
drop1(m4)
# None of the pairwise interactions was statistically significant.

# Final model
m1<-lmer(maillard~ 1 + storage + temp + water + (1|rep),data=milk)
# Anova, non sequential and using Satterthwaite's method
drop1(m1)
summary(m1)

#d) Present parameter estimates and post-hoc results (expected means, i.e., LS-means).
# Remember to include confidence intervals to quantify the uncertainty.
library(emmeans)

emmeans(m1, "water", by = c("temp"))
plot(emmeans(m1, "water", by = c("temp")))

emmeans(m1, "water", by = c("temp","storage"))
plot(emmeans(m1, "water", by = c("temp","storage")))

library(multcomp)
mult_temp <- glht(m1, linfct = mcp(temp = "Tukey")) #adjusted p-values by Tukey´s method
mult_wat <- glht(m1, linfct = mcp(water = "Tukey")) #adjusted p-values by Tukey´s method
mult_stor <- glht(m1, linfct = mcp(storage = "Tukey")) #adjusted p-values by Tukey´s method

summary(mult_temp)
summary(mult_wat)
summary(mult_stor)


par(mfrow=c(1,3))
par(mai=c(1,.65,1.25,.5)) # Use sufficiently large upper margin
plot(mult_temp, col=2:7)
plot(mult_wat, col=2:7)
plot(mult_stor, col=2:7)

par(mai=c(1,1,1,1))

#------end------
