# Code for lecture 12, based on eNote-12.R.
install.packages("nlme")
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

getwd()
options(width = 120)
# 0. Read, prepare data files and plot the data

# I. Fit a random effects model using function lmer() from lme4/lme4Test
#    Covariance with structure corresponding to Compound symmetry.

# II. Fit models with different covariance structures using function lme() from
#     library(nlme):
#       1) Compound symmetry
#       2) Gaussian 
#       3) Exponential
#       4) Linear
#       5) Symm
#       6) Autoregressive
# III. Option 3 was selected.

remove(list=ls())
#read data
rats<-read.csv("Data/rats.txt")
str(rats) #'data.frame':	300 obs. of  4 variables:
# file in long form

# make treatment and cage factors 
rats$treatm <- factor(rats$treatm)
rats$cage <- factor(rats$cage)

# make two versions of the time variable 
# - one quantitative and one factor
rats$monthQ <- as.numeric(rats$month)
rats$month <- factor(rats$month)
str(rats)


# Plots
p1<-ggplot(rats, aes(x=monthQ, y=lnc, group=cage, colour=treatm)) + 
  geom_point()+geom_line()
p1
mns <- ddply(rats, ~ treatm + month + monthQ, summarize, 
             lnc = mean(lnc))
p2<-ggplot(mns, aes(x=monthQ, y=lnc, group=treatm, colour=treatm)) + 
  geom_point() + geom_line()
p2

grid.arrange(p1, p2, ncol = 2) 
#-----------------------------
# I. Fit a random effects model using function lmer() from lme4/lme4Test
#    Covariance with structure corresponding to Compound symmetry.

m1 <- lmer(lnc ~ month + treatm + month:treatm + (1 | cage), data = rats)
m2 <- lmer(lnc ~ monthQ + treatm + monthQ:treatm + (1 | cage), data = rats)
m3 <- lmer(lnc ~ monthQ + I(monthQ^2) + treatm + monthQ:treatm + (1 | cage), data = rats)
anova(m1,m2) #prefers the largest model
anova(m2,m3) #prefers the largest model

residplot(m1)
residplot(m2)
residplot(m3)

ranova(m1)
ranova(m2)
ranova(m3)

anova(m1)
anova(m2)
anova(m3)
#------------
grid.arrange(p1, p2, ncol = 2) 
#m1: Interaction is significant and random effect too.
#m2: Interaction is significant and random effect too.
#m3: Interaction and quadratic term are significant and random effect too.

#For illustrative porpouses we use m1. Further analysis can be done
#using m2 or m3.
#--------------------------------------
# II. Fit models with different covariance structures using function lme() from
#     library(nlme):
#       1) Compound symmetry
#       2) Gaussian 
#       3) Exponential
#       4) Linear
#       5) Symm
#       6) Autoregressive

library(nlme)
?lme()
# Available standard classes(10):
  
# corAR1	autoregressive process of order 1.
# corARMA	autoregressive moving average process, with arbitrary orders for the autoregressive and moving average components.
# corCAR1	continuous autoregressive process (AR(1) process for a continuous time covariate).
# corCompSymm	compound symmetry structure corresponding to a CONSTANT correlation.
# corExp	exponential spatial correlation.
# corGaus	Gaussian spatial correlation.
# corLin	linear spatial correlation.
# corRatio	Rational quadratics spatial correlation.
# corSpher	spherical spatial correlation.
# corSymm	GENERAL correlation matrix, with no additional structure.

M<-list(10) #M[[1]]<-M1 #for a better programming??

#Fit model m1 with different error correlation structures
# and select one.

#1) Compound symmetry
M1<-lme(lnc ~ month + treatm + month:treatm,random = ~1|cage,
        correlation=corCompSymm(form=~monthQ|cage), data=rats)

anova(M1)
anova(M1, type="sequential")
anova(M1, type="marginal") #Type III.
anova(M1_ml, type = "marginal")
anova(update(M1, method="ML"), type = "marginal")

residplot(M1)
v1<-plot(Variogram(M1, form =~monthQ|cage,data = rats),
         cex=1,pch=16,ylim = c(0,2),xlab = "Distance. CompSymm????")
v1

#summary(M1)$tTable

# 2) Gaussian: Model with spatial Gaussian correlation:

M2<-lme(lnc~month+treatm+month:treatm,random=~1|cage,
              correlation=corGaus(form=~monthQ|cage),data=rats)
#anova(M2, type="marginal")
v2<-plot(Variogram(M2, form =~monthQ|cage,data = rats),
         cex=1,pch=16,ylim = c(0,2),xlab = "Distance. Gaussian")
v2

# 3) corExp	exponential spatial correlation:
M3<-lme(lnc~month+treatm+month:treatm,random=~1|cage,
        correlation=corExp(form=~monthQ|cage),data=rats)
v3<-plot(Variogram(M3, form =~monthQ|cage,data = rats),
         cex=1,pch=16,ylim = c(0,2),xlab = "Distance. Exponential")
v3

# 4) corLin	linear spatial correlation.
M4<-lme(lnc~month+treatm+month:treatm,random=~1|cage,
        correlation=corLin(form=~monthQ|cage),data=rats)
v4<-plot(Variogram(M4, form =~monthQ|cage,data = rats),
         cex=1,pch=16,ylim = c(0,2),xlab = "Distance. Linear")
v4

# 5) corSymm	general correlation matrix, with no additional structure.
M5<-lme(lnc~month+treatm+month:treatm,random=~1|cage,
        correlation=corSymm(form=~monthQ|cage),data=rats,
        control = lmeControl(msMaxIter = 200, niterEM = 50))
v5<-plot(Variogram(M5, form =~monthQ|cage,data = rats),
         cex=1,pch=16,ylim = c(0,2),xlab = "Distance. corSymm	general correlation matrix")
v5

#6) corAR1	autoregressive process of order 1.
M6<-lme(lnc~month+treatm+month:treatm,random=~1|cage,
        correlation=corAR1(0.5,form=~monthQ|cage),data=rats,
        control = lmeControl(msMaxIter = 200, niterEM = 50))
v6<-plot(Variogram(M6, form =~monthQ|cage,data = rats),
         cex=1,pch=16,ylim = c(0,2),xlab = "Distance. Autoregressive")
v6

gridExtra::grid.arrange(v1, v2,v3,v4,v5,v6, ncol = 2, nrow=3) 

#Candidates: 3)corExp, 5)corSymm, 6)corAR 

anova(M3, type = "marginal")
anova(M5, type = "marginal")
anova(M6, type = "marginal")


residplot(M3) #reasonable
residplot(M5) #less reasonable
residplot(M6) #reasonable

anova(M3,M5,M6) #According to the p-value the largest model should be 
                #selected: M5, But it has the largest AIC and BIC
anova(update(M3, method="ML"),update(M5, method="ML"),update(M6, method="ML")) #According to the p-value the largest model should be 
#selected: M5, But it has a very large BIC and 77 parameters!)
#M3 gives the same numerical values as M6.
summary(M3)
#(Intercept)  Residual
#StdDev:  0.07828243 0.2510377
summary(M5) 
#StdDev:   0.1511052 0.2112115
summary(M6)
#StdDev:  0.07828424 0.2510373
#Phi 
#0.754897 #AR(1) process with correlation in units per month,
# correspondint to .75 for 1 month lag.
#M3=M6?
par(mfrow=c(1,3))
plot(predict(M3, level=1),predict(M5, level=1), pch=20,
     main = "M3 Exponential vs M5 General")
abline(0,1, col="red")

plot(predict(M3, level=1),predict(M6, level=1), pch=20,
     main = "M3 Exponential vs M6 AR1")
abline(0,1, col="red")

plot(predict(M6, level=1),predict(M5, level=1), pch=20,
     main = "M6 AR1 vs M5 General")
abline(0,1, col="red")

#M3 Exponential and M6 AR1 has the same predicted values.

anova(update(M3, method="ML"),update(M6, method="ML")) #The same model

# M3 is a good candidate.
fixed.effects(M3)
random.effects(M3) #very small values relative to the fixed effects.

library(emmeans) #estimated response values
emmeans(M3, "treatm", by="month", data=rats)

#Further modelling:

#i) Try other correlation structures.
#ii) Try the same model with month as continuous (model m2).
# iii) Try month as continuous and its square, (model m3).

#m1 <- lmer(lnc ~ month + treatm + month:treatm + (1 | cage), data = rats)
#m2 <- lmer(lnc ~ monthQ + treatm + monthQ:treatm + (1 | cage), data = rats)
#m3 <- lmer(lnc ~ monthQ + I(monthQ^2) + treatm + monthQ:treatm + (1 | cage),
#             data = rats)
#---end------

