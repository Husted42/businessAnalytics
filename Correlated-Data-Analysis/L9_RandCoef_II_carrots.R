# See documentation for this example in eNote9 and eNote-13
rm(list=ls())
getwd()
library(lme4)
library(lmerTest)
library(lattice)
library(graphics)
library(latticeExtra)

library(predictmeans)
library(emmeans)
library(multcomp)
library(xtable)

# Carrots data:

carrots<-read.csv("https://www2.compute.dtu.dk/courses/02429/Data/datafiles/carrots.txt")
#carrots <- read.csv("Data/carrots.txt")
summary(carrots)
cor(carrots$sens1,carrots$sens2)
str(carrots)

carrots <- within(carrots, {
  Homesize <- factor(Homesize)
  Consumer <- factor(Consumer)
  product <- factor(product)
  Gender <- factor(Gender)
  })

str(carrots)
# Exploratory plots missing!

cor(carrots$sens1, carrots$sens2)
# randomizing product effect and modeling the effect of sens1 and sens2
# with random consumer slopes:
 
m1 <- lmer(Preference ~ Homesize + sens1 + sens2 +
Homesize * sens1 + Homesize * sens2 +
(1|product) + (1 + sens1 + sens2|Consumer),
data = carrots)
ranova(m1) #remove one random effect

m2 <- lmer(Preference ~ Homesize + sens1 + sens2 + 
                 Homesize * sens1 + Homesize * sens2 +
                 (1|product) + (1 + sens2| Consumer), data=carrots)

ranova(m2) #no more random effects to remove
#Fixed effects:
anova(update(m2,REML=F))

m3<-update(m2,.~.-Homesize:sens1)
anova(update(m3,REML=F))

m4<-update(m3,.~.-Homesize:sens2)
anova(update(m4,REML=F))

m5<-update(m4,.~.-sens1)
anova(update(m5,REML=F)) # A final candidate model

m5
anova(m5)
ranova(m5)
summary(m5)
confint(m5, oldNames=F)
residplot(m5)

str(ranef(m5))
par(mfrow=c(1,4))
qqnorm(resid(m5),main="Full residuals", pch = 19, cex = 0.75)
qqline(resid(m5), col="blue")

qqnorm(ranef(m5)$`Consumer`[,"(Intercept)"], main="Consumer/Intercept", pch=19, cex=0.75)
qqline(ranef(m5)$`Consumer`[,"(Intercept)"], col="blue")

qqnorm(ranef(m5)$`Consumer`[,"sens2"], main="Consumer/sens2", pch=19, cex=0.75)
qqline(ranef(m5)$`Consumer`[,"sens2"],col="blue")

qqnorm(ranef(m5)$`product`[,"(Intercept)"], main="Product/Intercept", pch=19, cex=0.75)
qqline(ranef(m5)$`product`[,"(Intercept)"],col="blue")
par(mfrow=c(1,1))

#Post-hoc analysis
formula(m5)
ems_size <- emmeans(m5, "Homesize")
ems_size
confint(pairs(ems_size))
# (for more digits, use the detaled estimators from the summary function).
#----end----

#See
step(m1)
#Model found:
# Preference ~ Homesize + sens2 + (1 | product) + (sens2 | Consumer)
#The same as m5.
#-----