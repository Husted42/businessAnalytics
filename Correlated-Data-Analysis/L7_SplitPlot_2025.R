# Based on eNote-7.R 
#Example I. Split-plot design, completely randomized
# Example II. Split-plot design with blocks.

remove(list=ls())

#Example I. Split-plot design, completely randomized

# Reading in data:

cooling<-read.csv("https://www2.compute.dtu.dk/courses/02429/Data/datafiles/cooling.txt")
str(cooling)


cooling$cooling<-factor(cooling$cooling)
cooling$phgroup<-factor(cooling$phgroup)
cooling$pigno<-factor(cooling$pigno)
head(cooling)
cooling
table(cooling$pigno) #2 obs per level. Balanced
table(cooling$phgroup) # 24 obs per level. Balanced
table(cooling$cooling) #24 obs per level. Balanced
ftable(cooling$cooling,cooling$phgroup,cooling$pigno)

library(lmerTest)
library(lattice)
library(lme4)
library(MASS)

library(predictmeans)
library(emmeans)
library(multcomp)

m0<-lmer(tender~cooling*phgroup + (1|pigno), data=cooling)
ranova(m0)
anova(m0)
residplot(m0)

# Interaction term is highly non significant

m1<-lmer(tender~cooling + phgroup + (1|pigno),data=cooling)
ranova(m1)
anova(m1)
residplot(m1)

anova(m1, m0) # p-value=.66


m2<-lmer(tender~ phgroup + (1|pigno),REML=T, data=cooling)
ranova(m2)
anova(m2)
summary(m2); confint(m2, oldNames=F)
residplot(m2)
(dotplot(ranef(m2, condVar=TRUE), strip = FALSE))

anova(m0,m1,m2)

# par(mfrow = c(1,2))
# qqnorm(resid(m2), pch = 19, cex = 0.75)
# qqline(resid(m2))
# str(ranef(m2))
# qqnorm(ranef(m2)$`pigno`[,"(Intercept)"], main="Random effects", pch=19, cex=0.75)
# qqline(ranef(m2)$`pigno`[,"(Intercept)"])
# par(mfrow = c(1,1))

# A box cox transformation does not sugests to transform.
# lambda-value one was within the confidence interval.


#Final model results
summary(m2)
(1.246+0.472) #1.78
1.246/(1.246+0.472) #=.72
confint(m2, oldNames=F)

# Basic post-hoc analysis does not give additional information
# since the final model has only one factor with two levels.
options(digits=4)
ls_means(m2)
ls_means(m2, pairwise=T)

# Example II. Split-plot design with blocks.

# Data:

oatyield<-read.csv("https://www2.compute.dtu.dk/courses/02429/Data/datafiles/oatyield.txt")
oats<-MASS::oats
?oats

str(oatyield)
summary(oatyield)
oatyield$block <- factor(oatyield$block)
oatyield$variety <- factor(oatyield$variety)
oatyield$fertil <- factor(oatyield$fertil)
str(oatyield)
str(oatyield)
#---------
#Plots
library(ggplot2)
p<-ggplot(aes(x=fertil, y=yield, group=variety, lty=variety),data=oatyield)
p<-p+geom_line() + facet_wrap(~block) + theme_bw()
p

par(mfrow=c(2,2))
par(mar=c(4,4,1,4))
with(oatyield, interaction.plot(fertil, variety, yield, legend=T, 
                   bty="n", col=2:11,  xtick = TRUE,type = "b",pch=19,
                   ylab = "Yield"))

with(oatyield,  interaction.plot(block, variety, yield, legend=T, 
                   bty="n", col=2:11, xtick = TRUE,type = "b",pch=19,
                   ylab = "yield"))

with(oatyield, interaction.plot(variety,fertil, yield, legend=T, 
                                bty="n", col=2:11,  xtick = TRUE,type = "b",pch=19,
                                ylab = "Yield"))

with(oatyield,  interaction.plot(variety,block, yield, legend=T, 
                                 bty="n", col=2:11, xtick = TRUE,type = "b",pch=19,
                                 ylab = "yield"))
#-----


m0<-lmer(yield ~ fertil + variety + fertil:variety + (1|block/variety),data=oatyield)
m0
#An alternative code for the same model
#m0_reml<-lmer(yield ~ fertil + variety + fertil:variety+(1|block)+
#               (1|block:variety),data=oatyield

# (1|block) indicates the block random effect, grouping factor.
#(1|block:variety) indicates the grouping factor which provides the
# number of different main units/plots in the data (18).
# 6 blocks
# Variety is the whole-plot factor with 3 levels.
# fertil is the subplot factor with 4 levels.

# corresponding fixed effects model:
ranova(m0)
anova(m0)
residplot(m0)

# Interaction highly non significant

m1<-lmer(yield ~ fertil + variety + (1|block/variety),data=oatyield)
ranova(m1)
anova(m1)

m2<-lmer(yield ~ fertil +(1|block/variety),data=oatyield)

anova(m2,m1) #p-value =.21
# variety could be removed, its effect is not statistically significant,
# but it may be left as it appears in the random effect.
# models m1 and m2 are both good proposed final model.
# We take m1.

anova(m1)
ranova(m1)
residplot(m1)
(dotplot(ranef(m1, condVar=TRUE), strip = FALSE))
# Block 1 has a very high random effect value

anova(m2)
ranova(m2)
residplot(m2)
(dotplot(ranef(m2, condVar=TRUE), strip = FALSE))
# Block 1 has a very high random effect value
par(mfrow=c(1,1))
plot(predict(m1), predict(m2), pch=19, cex=.5)
abline(0,1, col="red")
#Results:
emmeans(m1, "fertil", by=c("variety" ))
emmeans(m1, "fertil")
emmeans(m2, "fertil")

# Basic post-hoc analysis 
options(digits=4)
# Multiple testing and CI plots
library(multcomp)
mult_fertil <- glht(m1, linfct = mcp(fertil = "Tukey")) #adjusted p-values by Tukey´s method
mult_variety <- glht(m1, linfct = mcp(variety = "Tukey")) #adjusted p-values by Tukey´s method

summary(mult_fertil)
summary(mult_variety)


par(mfrow=c(1,2))
par(mai=c(1,.65,1.25,.5)) # Use sufficiently large upper margin
plot(mult_fertil, col=2:7)
plot(mult_variety, col=2:7)
par(mai=c(1,1,1,1))

#With model 2
m2_fertil <- glht(m2, linfct = mcp(fertil = "Tukey")) #adjusted p-values by Tukey´s method

summary(m2_fertil)

par(mfrow=c(1,1))
plot(m2_fertil, col=2:7)
#----end----


