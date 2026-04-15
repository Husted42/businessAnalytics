#
# 02429 - Analysis of correlated data: Mixed Linear Models    
#
#Week 1. Exercise. Observational data on Low Birth Weight.
#Risk Factors Associated with Low Infant Birth Weight
#The dataset has originally been retrieved from library(MASS),
# G. Eslava

library(MASS)
help("birthwt") 
library(emmeans) #For computing conditional/marginal means
library(multcomp) #For multiple hypothesis testing 

# Practicality --------------------------------------------------------------------

#Change working directory to where you are working
getwd()
#For example  (Mac)
#setwd("/Users/guiesl/Library/CloudStorage/Dropbox/DTU/DTU_2024/02429_LMM/R-code") 
#Verify
getwd()

# Alternatively at the top bar select:
# Session > Set Working Directory >to Source File Location 

ls()
remove(list=ls())
ls()

options(width = 120, digits=4)


#Read data set
lbw <- read.delim("data/lbw.txt")
str(lbw)

#set correct type to variables of interest
lbw$bwt<- as.numeric(lbw$bwt)
lbw$race <- as.factor(lbw$race)
lbw$smoke <- as.factor(lbw$smoke)
str(lbw)

# Modify labels for smoke
levels(lbw$smoke)
levels(lbw$smoke) <- c("no", "yes")
levels(lbw$smoke)

# See first and last few rows in the dataset.
head(lbw,n=3);tail(lbw,n=3)

#Explore the dataset
summary(lbw) #too much information

# summaries by race
by(lbw$bwt, lbw$race, summary)
(means<- tapply(lbw$bwt, lbw$race,  mean))

#Boxplots and Stripcharts for the groups derived from two variables
par(mfrow=c(1,2))
boxplot(bwt ~ smoke:race, data = lbw, xlab='Race and Smoke',
        ylab='Birth weight', las=1, col=2:4)
(means2 <- tapply(lbw$bwt, lbw$race:lbw$smoke,  mean))
points(1:6, means2, pch = 23, cex = 0.75, bg = "red")

stripchart(bwt~smoke:race, data=lbw, vertical=TRUE, method="jitter", xlab="Smoke/Race", 
           ylab="Birth weight",pch=16,cex=.75, las=1, col=2:4)
points(1:6, means2, pch = 17, cex = 1.5, col = "black")
abline(h = mean(lbw$bwt), lty = 2)

par(mfrow=c(1,1))

#Interaction plot
interaction.plot(lbw$race,lbw$smoke,lbw$bwt,fun=mean,
                 type=c("b"),ylab="Birth weight",las=1,lwd=2,
                 trace.label="Smoke",xlab="Race")

#Fit model with interaction
model2<-lm(bwt ~ race + smoke + race:smoke, data = lbw)

#see anova table 
anova(model2) #type I Sequential analysis of variance table
drop1(model2,test="F")  #type III: Single term deletions
# interaction term is not statistically significant
summary(model2)

#Fit reduced model
model3 <- lm(bwt ~ race + smoke, data = lbw)
drop1(model3, test="F")

#Diagnostic plots
#par(mai=c(1,1,1,1))
par(mfrow=c(2,2))
plot(model3, which = c(1:4), pch=16, cex=.5)
par(mfrow=c(1,1))

# The model fit looks satisfactory. Three observations might be of concern,
# one may have a look up on them.

# Note that the residuals vs fitted plot shows a large amount of spread, which
# leads us to conclude that race and smoke are not enough to predict birthweight.

summary(model3)

#95% CI
confint(model3)
cbind(coef(model3), confint(model3))

#Alternativelly (Post-hoc analysis):
#Conditional means:
emmeans(model3, "race", by="smoke") 
emmeans(model3, "smoke", by="race") 

#Marginal means:
emmeans(model3, "race") #Are these useful/interesting?
emmeans(model3, "smoke")  #Are these useful/interesting?


?emmeans

(m_tuk <- glht(model3, linfct = mcp(race = "Tukey"))) # Makes all pair comparisons and uses an adjusted p-value.
(m_dunnett <- glht(model3, linfct = mcp(race = "Dunnet"))) # Compares each "treatment" vs one single one ("control")
                                                           # and uses adjusted p-values.
summary(m_tuk)
summary(m_tuk, test = adjusted("bonferroni"))
summary(m_tuk, test = adjusted("holm"))

summary(m_dunnett)
summary(m_dunnett, test = adjusted("bonferroni"))
summary(m_dunnett, test = adjusted("holm"))

# Visualize the estimated marginal means


par(mfrow=c(1,2))
par(mai=c(1,1.5,1.25,.5)) # Use sufficiently large upper margin
plot(m_tuk, col=2:7)
plot(m_dunnett, col=2:7)
par(mfrow=c(1,2))
par(mai=c(.5,.5,.5,.5))


# Note on (ANOVA type I, anova() ) vs (ANOVA type III, drop1() ).
# Total sum of squares decomposition. First make models:
m_r<-lm(bwt ~ race, data = lbw)
m_rs<-lm(bwt ~ race+smoke, data = lbw)
m_rs_i <- lm(bwt ~ race+smoke+race:smoke, data = lbw)

# ANOVA Type I or Sequential.
# It shows the decomposition of the total sum of squares.
# This type is used to compare two nested models.
anova(m_r)
anova(m_rs)
anova(m_rs_i)
anova( m_rs,m_rs_i)

#ANOVA Type III or Single term deletion:
# Each row of the table compares two models,
# one with and one without the variable on the row.
drop1(m_r,test="F")
drop1(m_rs,test="F")
drop1(m_rs_i,test="F")

# We shall use mainly ANOVA type III: Single term deletions (drop1(model, test="F)). 
#ANOVA Type I will be used for COMPARING two or more models (anva(model1, model2))
#end ------------

