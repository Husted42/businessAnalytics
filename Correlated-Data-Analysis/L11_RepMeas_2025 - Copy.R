# Code for lecture 11.
getwd()
remove(list=ls())

library(nlme)
library(lme4)
library(lmerTest)


library(lattice)
library(latticeExtra)
library(predictmeans)
library(ggplot2)
library(gridExtra)
library(plyr)
library(multcomp)
library(emmeans)
library(effects)
library(doBy)
library(xtable)

options(width = 140)
# 0. Read, prepare datafiles and plot the data

# I. Fit a random effects model using function lmer() from lme4/lme4Test
#    Covariance with structure corresponding to Compound symmetry.

#0.read data
rats<-read.csv("Data/rats.txt") #file in long format
str(rats) #'data.frame':	300 obs. of  4 variables:
# file already in long form

# make treatment and cage factors 
rats$treatm <- factor(rats$treatm)
rats$cage <- factor(rats$cage)

# make two versions of the time variable 
# - one quantitative and one qualitative
rats$monthQ <- rats$month
rats$month <- factor(rats$month)
#-------

rats$monthQ <- as.numeric(rats$month)
p1<-ggplot(rats, aes(x=monthQ, y=lnc, group=cage, colour=treatm)) + 
  geom_line()
p1
mns <- ddply(rats, ~ treatm + month + monthQ, summarize, 
             lnc = mean(lnc))
p2<-ggplot(mns, aes(x=monthQ, y=lnc, group=treatm, colour=treatm)) + 
  geom_point() + geom_line()
p2

grid.arrange(p1, p2, ncol = 2) 

# I. Fit a random effects model using function lmer() from lme4/lme4Test
#    Covariance with Compound-symmetry structure.

m1 <- lmer(lnc ~ month + treatm + month:treatm + (1 | cage), data = rats)
ranova(m1)
anova(m1) #Type III Analysis of Variance Table with Satterthwaite's method
#Interaction is significant and random effect too.
summary(m1) 
ranef(m1)
dotplot(ranef(m1, condVar=TRUE), strip = FALSE)
residplot(m1)
#
m2 <- lmer(lnc ~ monthQ + treatm + monthQ:treatm + (1 | cage), data = rats)
ranova(m2)
anova(m2) 
summary(m2) 
ranef(m2)
dotplot(ranef(m2, condVar=TRUE), strip = FALSE)
residplot(m2)

m3 <- lmer(lnc ~ monthQ + I(monthQ^2) + treatm + monthQ:treatm + (1 | cage), data = rats)
ranova(m3)
anova(m3) 
summary(m3) 
ranef(m3)
dotplot(ranef(m3, condVar=TRUE), strip = FALSE)
residplot(m3)

anova(m1,m2,m3)
#Which model? 
# It depends of the use and user of the model results.
# m1 has the smallest AIC but the largest BIC, 32 parameters..
# m3 has the second smallest AIC and the smallest BIC, with 9 parametrs.

par(mfrow=c(1,3))
plot(rats$lnc, predict(m1), pch=20)
abline(0,1, col="red")
plot(rats$lnc, predict(m2), pch=20)
abline(0,1, col="red")
plot(rats$lnc, predict(m3), pch=20)
abline(0,1, col="red")
#------
#post-hoc e.g. Be aware of interactions.
formula(m1) #lnc ~ month + treatm + month:treatm + (1 | cage)

emmeans(m1,  ~ month:treatm)
emmeans(m1,  ~ month:treatm, at = list(month =c("1","5", "10")))
emmeans(m1,  ~ treatm)
emmeans(m1,  ~ treatm, at = list(month = "4"))

#--------------------------------------
#Three functions for fitting a model with compound-symmetry variance structure.
#with library(nlme)

m1 <- lmer(lnc ~ month + treatm + month:treatm + (1 | cage),REML=T, data = rats)
m1_nlme<-lme(lnc ~ month + treatm + month:treatm,random = ~1 | cage,method = "REML", data = rats)
m1_gls<-gls(lnc~month+treatm+month:treatm,
                     correlation=corCompSymm(form=~1|cage),
                     method="REML",data=rats)

logLik(m1) #'log Lik.' -4.307319 (df=32)
logLik(m1_nlme) #idem
logLik(m1_gls)  #idem

m1
m1_nlme
m1_gls

summary(m1)
summary(m1_nlme)
summary(m1_gls)
#generalized least squares gls().
#This function fits a linear model using generalized least squares. 
#The errors are allowed to be correlated and/or have unequal variances.
#It allows to fit models with correlated errors and with no random effects.
# lme() on lmer() do not allow models with no random effect.
#----end------