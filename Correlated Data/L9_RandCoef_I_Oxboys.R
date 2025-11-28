#Two examples
 
#Example 1 from  Aitkin et al. Statistical  Modelling in R. Ch.9
#Example 2, code based on eNote-9 and Anders Stockmar lecture.

remove(list=ls())
# install.packages("latticeExtra")
library(lme4)
library(lmerTest)
library(lattice)
library(graphics)
library(latticeExtra)

library(predictmeans)
library(emmeans)
library(multcomp)
library(xtable)

df<-nlme::Oxboys
str(df) #234 obs. of  4 variables
#age has been transformed to age -13
summary(df) #balanced data
9*26 #234
head(df, n=12)
df<-as.data.frame(df[,1:4])
head(df, n=12)
tail(df)

xyplot(height ~ I(age+13), xlab="age",group= Subject, type="b", lty=3,
       pch=c(0:25), cex=.75, data=df)



#Start with a model with random intercept
m1_reml<-lmer(height ~ 1 + age + (1|Subject), data=df)
m1_reml

ranova(m1_reml) #random effect statistically significant
anova(m1_reml)
residplot(m1_reml) # not unreasonably

summary(m1_reml)
65.555/(65.555+1.718) #0.9744623 Intra-class correlation is very high
# i.e. thw between subjects variance accounts for 97% of the total variance 
# of the response variable. Only 3% is explained by the within subjects variablility.

confint(m1_reml,oldNames=F)

dotplot(ranef(m1_reml, condVar=TRUE), strip = FALSE)

#A model with random intercept and random slope

m2_reml<-lmer(height ~ 1 + age + (1 + age|Subject), data=df)
#m2_ml<-lmer(height ~ 1+age + (1+age|Subject),REML=F, data=df)

ranova(m2_reml)
anova(m2_reml)
residplot(m2_reml)

par(mfrow=c(1,3))
qqnorm(resid(m2_reml),main="Full residuals", pch = 19, cex = 0.75)
qqline(resid(m2_reml))
str(ranef(m2_reml))
qqnorm(ranef(m2_reml)$`Subject`[,"(Intercept)"], main="Intercept", pch=19, cex=0.75)
qqline(ranef(m2_reml)$`Subject`[,"(Intercept)"])
qqnorm(ranef(m2_reml)$`Subject`[,"age"], main="age", pch=19, cex=0.75)
qqline(ranef(m2_reml)$`Subject`[,"age"])

summary(m2_reml)
confint(m2_reml, oldNames=F)
anova(m1_reml, m2_reml) #OBS:refitting model(s) with ML (instead of REML)

#both random effects and fixed effects are stat. significant

par(mfrow=c(1,1))
dotplot(ranef(m2_reml, condVar=TRUE), strip = FALSE)


fixef(m2_reml) # Fixed coefficients
ranef(m2_reml) #26 pairs of random effects, one per boy

# Model with random intercept and random slope,
# adding age^2 as a term with non-random associated slope

m3_reml<-lmer(height ~ 1+age + I(age^2) + (1+ age|Subject), data=df)
#m3_ml<-lmer(height ~ 1+age + I(age^2) + (1+age|Subject), REML=F, data=df)

ranova(m3_reml)
anova(m3_reml)
residplot(m3_reml)

par(mfrow=c(1,3))
qqnorm(resid(m3_reml),main="Full residuals", pch = 19, cex = 0.75)
qqline(resid(m3_reml))
str(ranef(m2_reml))
qqnorm(ranef(m3_reml)$`Subject`[,"(Intercept)"], main="Intercept", pch=19, cex=0.75)
qqline(ranef(m3_reml)$`Subject`[,"(Intercept)"])
qqnorm(ranef(m3_reml)$`Subject`[,"age"], main="age", pch=19, cex=0.75)
qqline(ranef(m3_reml)$`Subject`[,"age"])

summary(m3_reml)

confint(m3_reml, oldNames=F)

anova(m3_reml, m2_reml) #Obs refitting model(s) with ML (instead of REML)

#age^2 statistically significant

dotplot(ranef(m3_reml, condVar=TRUE), strip = FALSE)

par(mfrow=c(1,1))
plot(predict(m2_reml), predict(m3_reml), pch=19, cex=.5)
abline(0,1, col="red")

predict(m3_reml)
formula(m3_reml)
#See values for boy 1 initial height:
df[1,]        #140.5 Observed height at age=-1
#Obs age=-1
predict(m3_reml)[1]  #141.4 predicted height initial height

ranef(m3_reml) #-1.2474  0.61212
fixef(m3_reml) #149.0614      6.5168      0.7419 

(149.0614 - 1.2474) +  (6.5168+0.61212)*(-1) + 0.7419*((-1)^2) #=141.4

# Model with random intercept and random slope,
# adding age^2 as a term with random slope

m4_reml<-lmer(height ~ 1+ age + I(age^2) + (1+age+ I(age^2)|Subject), data=df)
#m4_ml<-lmer(height ~ 1+age + I(age^2) + (1+age+ I(age^2)|Subject), REML=F, data=df)

ranova(m4_reml)
anova(m4_reml)

residplot(m4_reml)

summary(m4_reml)
confint(m4_reml, oldNames=F) #There were 50 or more warnings (use warnings() to see the first 50)

anova(m4_reml, m3_reml) #refitting model(s) with ML (instead of REML)

dotplot(ranef(m3_reml, condVar=TRUE), strip = FALSE)

par(mfrow = c(1,2))

par(mfrow=c(1,4))
qqnorm(resid(m4_reml),main="Full residuals", pch = 19, cex = 0.75)
qqline(resid(m4_reml))
str(ranef(m4_reml))
qqnorm(ranef(m4_reml)$`Subject`[,"(Intercept)"], main="Intercept", pch=19, cex=0.75)
qqline(ranef(m4_reml)$`Subject`[,"(Intercept)"])
qqnorm(ranef(m4_reml)$`Subject`[,"age"], main="age", pch=19, cex=0.75)
qqline(ranef(m4_reml)$`Subject`[,"age"])
qqnorm(ranef(m4_reml)$`Subject`[,"I(age^2)"], main="age^2", pch=19, cex=0.75)
qqline(ranef(m4_reml)$`Subject`[,"I(age^2)"])
par(mfrow=c(1,1))

anova(m1_reml,m2_reml, m3_reml, m4_reml)
#m4 too many parameters
##There were 50 or more warnings (use warnings() to see the first 50) at
# confint(m4_reml)

#We select m3_reml as final model.
df$m3<-149.0614 + 6.5168*df$age + 0.7419*(df$age^2)
par(mfrow=c(1,1))
with(df,{
  plot(age, height, las=1,xlab="age-13",ylab="height",ylim=c(120,180), xlim=c(-1,1.4))
  for(iboy in 1:26) lines(age[Subject==iboy], height[Subject==iboy],
                          col=iboy-1,lty=iboy-1)
})
lines(df$age[1:9],df$m3[1:9], col="red", lty=1, lwd=2)

par(mfrow=c(1,1))
with(df,{
  plot(age, predict(m3_reml), las=1,xlab="age-13",ylab="fitted height (m3_reml)",
       ,ylim=c(120,180), xlim=c(-1,1.4))
  
  #axis(1, at = -1:1.4)
  for(iboy in 1:26) lines(age[Subject==iboy], height[Subject==iboy],
                          col=iboy-1,lty=iboy-1)
})
lines(df$age[1:9],df$m3[1:9], col="red", lty=1, lwd=3)
abline(v=1.3, lty=3, col="blue")
par(mfrow=c(1,1))
#-----end-----

#If you fit a linear model height = alpha + beta_1 age + beta_2 age^2 + epsilon
# and compute the average of the 24 intercepts, 24 beta_1, and 24 beta_2.
#Then compare these averaged coefficients with those obtained by the model
# with the same fixed effects plus random effects, in this example, these
# compared values are very close.

df$agesq<-df$age^2
mC_lm<-lm(height~ -1+ Subject +age:Subject + agesq:Subject, data = df)
summary(mC_lm)
(cbind(coef(mC_lm)))
dim((cbind(coef(mC_lm))))

(lm_intercept_mean<-mean(coef(mC_lm)[1:26]))
(lm_intercept_sd<-sd(coef(mC_lm)[1:26])/sqrt(26))

(lm_slope_mean<-mean(coef(mC_lm)[27:52]))
(lm_slope_sd<-sd(coef(mC_lm)[27:52])/sqrt(26))

(lm_slope2_mean<-mean(coef(mC_lm)[53:78]))
(lm_slope2_sd<-sd(coef(mC_lm)[53:78])/sqrt(26))
#-----------

