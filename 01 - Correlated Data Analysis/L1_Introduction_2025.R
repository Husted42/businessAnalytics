#
# 02429 - Analysis of correlated data: Mixed Linear Models     #
# R-script for Lecture 1: Recapitulation of LM.Two examples.  #
#
# I.   Example 1. Experimental data on Absorbed fat.
# II. Example 2. Grouped or clustered observations (Bulls).
# G. Eslava
#---------------------------------------------------------------
?options
getOption("contrasts")
getOption("digits")
getOption("scipen")

# Practicality --------------------------------------------------------------------

#Make sure you are at the working directory you are working
getwd()
#For example  (Mac)
setwd("/Users/guiesl/Library/CloudStorage/Dropbox/DTU/DTU_2024/02429_LinearMixedModels/R-code") 
#Verify
getwd()

# Alternatively at the top bar select:
# Session > Set Working Directory >to Source File Location 

#clean the environment
ls()
remove(list=ls())
ls()

options(width = 120, digits=4)

library(MASS)
library(emmeans) #For computing conditional/marginal means
library(multcomp) #For multiple hypothesis testing 

# Example 1. Experimental data on Absorbed fat. ---------------------------

# One-way analysis of variance (ANOVA) with balanced data

# Type in data directly for each variable name
grams <- c(64,72,68,77,56,95, 78, 91, 97, 82, 85, 77,75,93,78,71,63,76,55,66,49,64,70,68)  
fattype<- rep(c("A","B","C","D"), each = 6)
#fattype<- c("A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B", "C", "C", "C", "C", "C", "C", "D", "D", "D", "D", "D", "D")

# Make a dataframe (a table with both columns)
fatdata<- data.frame(fattype, grams) 
fatdata

# See the structure of the data set
str(fatdata) 

# Change fattype to factor (categorical variable)
fatdata$fattype<- as.factor(fatdata$fattype)
str(fatdata) 

# See summary of data
summary(fatdata)

# See summary of data by each category of variable
summary_var<-with(fatdata, by(grams, fattype, summary)) 
summary_var

# Get means by groups
?tapply
?aggregate
(means<-tapply(fatdata$grams, fatdata$fattype, mean))
(means_agg<- aggregate(fatdata$grams, list(fatdata$fattype), mean))

str(means)
str(means_agg)

means
means_agg

means[1]^2
means_agg[1,2]^2

# Exploratory plots
# Boxplot:
boxplot(grams ~ fattype, data = fatdata, xlab='Fat type', ylab='Absorbed fat', 
        las=1, col=2:5)
points(1:4, means, pch = 23, cex = 0.95, bg = "red")

# Stripchart:
stripchart(grams ~ fattype, data = fatdata, xlab='Fat type', ylab='Absorbed fat', 
           vertical=TRUE,cex=1.2,pch=16, las=1, col=2:5)
points(1:4, means, pch = 17, cex = 1.2, col = "black")
abline(h = mean(fatdata$grams), lty = 2)

# Graph them side by side with par(mfrow), ?par
par(mfrow=c(1,2))
boxplot(grams ~ fattype, data = fatdata, xlab='Fat type', ylab='Absorbed fat', 
        las=1, col=2:5)
points(1:4, means, pch = 23, cex = 0.95, bg = "red")
stripchart(grams ~ fattype, data = fatdata, xlab='Fat type', ylab='Absorbed fat', 
           vertical=TRUE,cex=1.2,pch=16, las=1, col=2:5)
points(1:4, means, pch = 17, cex = 1.2, col = "black")
abline(h = mean(fatdata$grams), lty = 2)
par(mfrow=c(1,1))

# Fit a Linear Model (LM)
model1 <- lm(grams ~ fattype, data = fatdata)

# Summary for parameter estimates and F-statistic
summary(model1)

# See the ANOVA for factor/covariate significance
anova(model1)             # sequential sum of squares
drop1(model1, test = "F") # Single term deletions, partial sum of squares

# The sequential and single-term deletion are the same for models with
# one single explanatory variable.
# With models with more than one explanatory variable only 
# their last row are equal.

# Linear Model without intercept
# Convenient mainly for getting mean estimates directly
model1B <- lm(grams ~ fattype - 1, data = fatdata) 
summary(model1B)

#Or use library(emmeans) -estimated marginal means.
#help(package="emmeans")
emmeans(model1,"fattype")

# To visualize the lsmeans library(multcomp) 
 
(tuk <- glht(model1, linfct = mcp(fattype = "Tukey"))) # Makes all pair comparisons and uses an adjusted p-values.
(dunn <- glht(model1, linfct = mcp(fattype = "Dunnet"))) # Compares each "treatment" vs one single one ("control")
                                                         # and uses adjusted p-values.
par(mfrow=c(1,2))
par(mai=c(1,1,1.25,.5)) # Use sufficiently large upper margin
plot(tuk, col=2:5)
plot(dunn, col=2:5)
par(mai=c(1,1,1,1))

?glht #General Linear Hypotheses tests

#At least three methods for adjusting the p-values.
#Estimates and se errors are the same.
summary(tuk)
summary(tuk, test = adjusted("bonferroni"))
summary(tuk, test = adjusted("holm"))

summary(dunn)
summary(dunn, test = adjusted("bonferroni"))
summary(dunn, test = adjusted("holm"))


#Structure of the object model1 where the model results are stored:
str(model1) 
# e.g.:
model1$rank #number of parameters in the model (4)
model1$rank
model1$coefficients
model1B$coefficients


#Observe that fitted values obtained by model1 and model1B are identical

plot(model1$fitted.values, model1B$fitted.values)
abline(0,1, col="red")

(sum(model1$fitted.values - model1B$fitted.values))

# Technical note. See change of reference group to group D
# i) reate a new variable:
fatdata$newfattype <- relevel(fatdata$fattype, ref="D")
levels(fatdata$newfattype)

model1_D<-lm(grams ~ newfattype, data = fatdata)
summary(model1_D)

# Look at the diagnostic plots
par(mai=c(.5,.7,.5,.5))
par(mfrow = c(2,2))
plot(model1, which=1:4, pch=19, cex=.5)
par(mfrow = c(1,1))
# If a model fits, the reiduals should distributed approximately normal
# with zero mean and constant variance:

# Residual vs fitted: distributed around the horizontal line at zero vertical axe,
# else they can indicate nonindependence or non constant variance
# Q-Q plot: Close to a straight line, if they are approximatelly normally distributed
# Scale-location:  residual magnitudes within a band, else an indication of a non-constant variance.
# Cook´s distance plot: Observations that might be influential.
#------------------------------------------------------------------

# Example 2. Grouped or clustered observations (Bulls). 

# Example 13.7.1,Snedecor and Cochran, Statistical Methods, p 246
# Illustrative example on Fixed and Random effects

rm(list=(ls()))

options(width = 160, digits = 4)

#Read in the data
perc<-c(46,31,37,62,30,
        70, 59,
        52,44,57,40,67,64,70,
        47,21,70,46,14,
        42,64,50,69,77,81,87,
        35,68,59,38,57,76,57,29,60)
bull<-c(1,1,1,1,1,
        2,2,
        rep(3,7),
        rep(4,5),
        rep(5,7),
        rep(6, 9))

df<-data.frame(bull, perc)
df
str(df)
df$bull<- as.factor(df$bull)
str(df)

# Exploratory analysis

(mean<-mean(df$perc))
(means<-with(df,tapply(perc, bull, mean)))
tapply(df$perc, df$bull, mean)

(s2<-var(df$perc))
#(s2_i<-(vars<-tapply(df$perc, df$bull, var)))
(s2_i<-tapply(df$perc, df$bull, var))
(n_i<-table(df$bull))

cbind(n_i, means, s2_i)

# Exploratory plots
par(mfrow=c(1,2))
with(df, boxplot(perc ~ bull, xlab='bull', ylab='% of conceptions', las=1, col=2:5))
#alternatively:
# boxplot(df$perc ~ df$bull, xlab='bull', ylab='% of conceptions', las=1, col=2:5)
points(1:6, means, pch = 23, cex = 0.95, bg = "red")

with(df, stripchart(perc ~ bull, xlab='bull', ylab='% of conceptions', 
                    vertical=TRUE,cex=1.2,pch=16, las=1, col=2:5))
points(1:6, means, pch = 17, cex = 1.5, bg = "black")
abline(h=mean(df$perc), lty = 2)
par(mfrow=c(1,1))

# Fitting three models

# Model 1. Simple fixed effects model
m_lm_null <- lm(perc~1,data=df)
anova(m_lm_null)
drop1(m_lm_null)
summary(m_lm_null)
#estimated mean = 53.54 = observed mean
confint(m_lm_null, level=0.95)
cbind(coef(m_lm_null), confint(m_lm_null, level=0.95))

mean(df$perc)
sqrt(var(df$perc)/35)

# Model 2. Fixed effects model, with bulls as the fixed effect factor
m_lm <- lm(perc~bull,data=df)
anova(m_lm)
drop1(m_lm,test="F")
summary(m_lm)

# factor bull is statistically significant,
# i.e. On average the percentage of conception of the six bulls differs.
# at least two bulls have a statistically different conception percentage.

summary(m_lm)
confint(m_lm)
#F-statistic: 2.68 on 5 and 29 DF,  p-value: 0.0441
# Bull 5 and 1 have a statistically different percentage of conceptions.

#See estimated means directly:
summary(lm(perc~bull-1,data=df))

#Alternativelly:
emmeans(m_lm,"bull")
# Visualize the estimated marginal means

(tuk <- glht(m_lm, linfct = mcp(bull = "Tukey"))) ## S3 method for class 'mcp'

par(mai=c(1,1,1.25,.5)) # Use sufficiently large upper margin
plot(tuk, col=2:7)
par(mai=c(.5,.5,.5,.5))

#Diagnostic plots
par(mfrow=c(2,2))
plot(m_lm, which = c(1:4), pch=16, cex=.75)
par(mfrow=c(1,1))
#Model fitting looks acceptable
#Notice that the predictive power of the model is low.
# Descriptive vs predictive model

plot(df$perc, m_lm$fitted.values, col=df$bull, pch=19)
abline(0,1)

# Model 3. Random effects model with bulls as random effect

# Now we assume that the six bulls are a random sample from a large population
# of bulls and that one wishes to estimate the average percentage of conceptions,
# and that the percentages of conceptions of the bulls are on average the same 
# in the population.
# The observations within each bull are expected to be correlated.
# The observations between bulls are expected to be uncorrelated.

#install.packages("lme4")
library(lme4)

m_lmer<-lmer(perc ~ (1|bull), data=df)
m_lmer<-lmer(perc ~ 1 + (1|bull), data=df)
#str(m_lmer) #A lot of information!
#m_lmer@beta

summary(m_lmer)


76.8+248.7 #=325.5

76.8/(76.8+248.7) #= 0.24
#variance due to the bulls:76.8
# Total variance= 76.8+248.7=325.5

#Fixed effects: none!
drop1(m_lmer, test="Chisq") 
anova(m_lmer)

# Random effects 
#See confidence intervals:
confint(m_lmer, oldNames=F)

cbind(coef(m_lm_null), confint(m_lm_null, level=0.95))

#Notice that the CI are wider in the random effects setting than in the fixed.

#-----end--------
#install.packages("lattice")
library(lattice)
# See the ranking og the percentage of conceptions of the bulls.

dotplot(ranef(m_lmer, condVar=TRUE), strip = FALSE)

predict(m_lmer)
par(mai=c(1,1,1,1))
plot(predict(m_lmer), m_lm$fitted.values, col=df$bull, pch=19)
abline(0,1)

str(ranef(m_lmer))

par(mfrow=c(1,2))
qqnorm(ranef(m_lmer)$bull[,"(Intercept)"], main="Random effects", pch=19, cex=.5)
qqline(ranef(m_lmer)$bull[,"(Intercept)"])
qqnorm(resid(m_lmer), main="Residuals",pch=19, cex=.5)
qqline(resid(m_lmer))  

#----end----

