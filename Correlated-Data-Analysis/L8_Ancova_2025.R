# Code based on eNote-8.R

# Three examples as illustrated in eNote-8
#I. Example with equal slopes anova.
# Hormone treatment of steers. Randomized blocks design (kidney).
#II. Example with unequal slopes anova. Balanced incomplete block (BIB) design.

remove(list=ls())

library(lmerTest)
library(lattice)
library(lme4)
library(MASS)

library(predictmeans)
library(emmeans)
library(multcomp)
library(xtable)

getwd()
# kidney <- read.table("data/kidney.txt", sep=" ", header=TRUE)
kidney<-read.table("http://www2.compute.dtu.dk/courses/02429/Data/datafiles/kidney.txt",header=TRUE,sep="")
str(kidney)

kidney$weight<- as.numeric(kidney$weight)
kidney$Y<- as.numeric(kidney$Y)
kidney$block <- factor(kidney$block)
kidney$treat <- factor(kidney$treat)

str(kidney)
# 16 observations only!
kidney
table(kidney$block) #balanced
table(kidney$treat) #balanced



par(mfrow=c(1,2))
with(kidney, {
  plot(weight, Y, type="n", xlab = "Weight", ylab = "Y", las=1) 
  points(weight[treat == 1], Y[treat == 1], pch = "1", col=1)
  points(weight[treat == 2], Y[treat == 2], pch = "2", col=2)
  points(weight[treat == 3], Y[treat == 3], pch = "3", col=3)
  points(weight[treat == 4], Y[treat == 4], pch = "4", col=4)
})


####
with(kidney, {
  plot(weight, Y, type = "n", xlab = "Weight", ylab = "Y", las = 1, 
       main="four independent lines")
  points(weight[treat == 1], Y[treat == 1], pch = "1", col = 1)
  points(weight[treat == 2], Y[treat == 2], pch = "2", col = 2)
  points(weight[treat == 3], Y[treat == 3], pch = "3", col = 3)
  points(weight[treat == 4], Y[treat == 4], pch = "4", col = 4)
  
  # Add independent regression lines for each treatment
  abline(lm(Y[treat == 1] ~ weight[treat == 1]), col = 1, lwd = 1)
  abline(lm(Y[treat == 2] ~ weight[treat == 2]), col = 2, lwd = 1)
  abline(lm(Y[treat == 3] ~ weight[treat == 3]), col = 3, lwd = 1)
  abline(lm(Y[treat == 4] ~ weight[treat == 4]), col = 4, lwd = 1)
  
  legend("topleft", legend = paste("Treat", 1:4), col = 1:4, lty = 1, bty = "n", cex=.65)
})

# ANCOVA will fit 4 regressions simultaneously using all the observations
#------Models

#---- Model with treatment as single variable-
m0 <- lmer(Y ~ treat + (1|block), data = kidney)
ranova(m0)
anova(m0)

m0_lm <- lm(Y ~ treat , data = kidney)
anova(m0_lm)
#Treatment effect is not significant
# the LMM and the LM model.
#--------------
# Model with a covariate. Different slopes (ineractions). Ancova.
m0 <- lmer(Y ~ treat*weight + (1|block), data = kidney)
ranova(m0)
anova(m0) #H_0: #H_0: beta_1=beta_2=beta_3=beta_4

# The random effect is not significant, nor the interaction.

m1<-lm(Y ~ treat + weight + treat:weight, data = kidney)
# random effect is not statistically significant
drop1(m1, test="F") # both effects are highly statistically significant
# Interaction non significant.

# an ANCOVA model with no random effects.

m1_lm <- lm(Y ~ treat + weight, data = kidney)
drop1(m1_lm, test="F")
# both effects are statistically significant.
summary(m1_lm)
# we could use an alternative parametrization of the same model
# ONLY to get the intercepts and the slope for the four lines,
#  it is convenient for plotting the estimated lines, NOT for testing:

m1B_lm <- lm(Y ~ treat + weight -1, data = kidney)
drop1(m1B_lm, test="F")
summary(m1B_lm)

coef(m1_lm)
# same estimated coefficient values as when using block as random effect.
# differenc Confidence intervals.

## -----
#Diagnostic plots
#par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1.5,1))

residplot(m1_lm)
par(mfrow=c(1,1))

with(kidney, {
  plot(weight, Y, type="n", xlab = "Weight", ylab = "Y", las=1) 
  points(weight[treat == 1], Y[treat == 1], pch = "1", col=1)
  points(weight[treat == 2], Y[treat == 2], pch = "2", col=2)
  points(weight[treat == 3], Y[treat == 3], pch = "3", col=3)
  points(weight[treat == 4], Y[treat == 4], pch = "4", col=4)
})

xaux<-seq(from = 350, to = 700, length.out=20)

lines(xaux, coef(m1B_lm)[1] + coef(m1B_lm)[5]*xaux, col=1, type="l",lty=5)
lines(xaux, coef(m1B_lm)[2] + coef(m1B_lm)[5]*xaux, col=2, type="l",lty=5)
lines(xaux, coef(m1B_lm)[3] + coef(m1B_lm)[5]*xaux, col=3, type="l",lty=5)
lines(xaux, coef(m1B_lm)[4] + coef(m1B_lm)[5]*xaux, col=4, type="l",lty=5)
abline(v=mean(kidney$weight),lty=2)
#--------

summary(m1_lm) #intercept for treat 1 is statistically equal to the one for treat 2.
summary(m1B_lm)
summary(m1_lm)$sigma^2 #126.1

## ----Means comparison--------------------------------------------
summary(kidney$weight)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#360.0   427.5   465.0   477.5   515.0   690.0 

emmeans(m1_lm, "treat", by="weight", 
        at=list(weight=c(quantile(kidney$weight,.25), mean(kidney$weight), quantile(kidney$weight,.75))))

emmeans(m1_lm, "treat", by="weight")

pairs(emmeans(m1_lm, ~ treat, at = list(weight=mean(kidney$weight) )))
# difference  between treat 1 and 4; and 1 and 3.
summary(m1_lm) # difference  between treat 1 and 4; and 1 and 3.

#-------end Example I----


#II Example with unequal slopes. Balanced incomplete block (BIB) design.

remove(list=ls())

bib <- read.table("data/bib.txt", sep=" ", header=TRUE)
str(bib)
#24 obseravtions
bib
#xtable(bib)

bib$blk <- factor(bib$blk)
bib$trt <- factor(bib$trt)

par(mfrow=c(1,1))
with(bib, {
  plot(x, y, type="n", xlab = "x", ylab = "y", las=1)
  points(x[trt == 1], y[trt == 1], pch = "1", col=1)
  points(x[trt == 2], y[trt == 2], pch = "2", col=2)
  points(x[trt == 3], y[trt == 3], pch = "3", col=3)
  points(x[trt == 4], y[trt == 4], pch = "4", col=4)
})

# model with interaction
# keep in mind that the number of obs is limited to 24

m0_reml <- lmer(y ~  trt+x+trt:x +(1|blk), data = bib)
ranova(m0_reml) # random effect significant
anova(m0_reml) # Interaction is significant

#Diagnostic plot
#par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1.5,1))

residplot(m0_reml)

summary(m0_reml)

#Use an alternative parametrization ONLY to get the intercepts and
# slope of the four lines.

m0B_reml <- lmer(y ~  trt + x:trt -1 +(1|blk), data = bib)
summary(m0B_reml)
round(fixef(m0B_reml), digits=2)
confint(m0B_reml)

plot(predict(m0_reml),predict(m0B_reml)) # Same models
abline(0,1)

sum(predict(m0_reml)-predict(m0B_reml))
logLik(m0_reml)
logLik(m0B_reml)

#Note:
# lmer(y ~  trt + x:trt -1 +(1|blk), data = bib) is the same model as
# lmer(y ~  trt*x +(1|blk), data = bib). They produce the same fitted values.
# They parametrize differently. The values of the coefficients from one model can be derived
# for the other's

fixef(m0_reml)

fixef(m0B_reml)

#plot
with(bib, {
  plot(x, y, type="n", xlab = "x", ylab = "y", las=1)
  points(x[trt == 1], y[trt == 1], pch = "1", col=1)
  points(x[trt == 2], y[trt == 2], pch = "2", col=2)
  points(x[trt == 3], y[trt == 3], pch = "3", col=3)
  points(x[trt == 4], y[trt == 4], pch = "4", col=4)
})

B <- fixef(m0B_reml)
with(bib, { # add regression lines by trt:
  abline(a=B["trt1"], b=B["trt1:x"], lty=5, col=1)
  abline(a=B["trt2"], b=B["trt2:x"], lty=5, col=2)
  abline(a=B["trt3"], b=B["trt3:x"], lty=5, col=3)
  abline(a=B["trt4"], b=B["trt4:x"], lty=5, col=4)
})
abline(v=min(bib$x), lty=3)
abline(v=mean(bib$x), lty=3)
abline(v=max(bib$x), lty=3)

formula(m0_reml)
summary(bib$x)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#11.0    17.5    28.5    26.0    37.0    39.0 

emmeans(m0_reml, "trt", by="x") # by default takes at=list(x=c(11, x=mean(bib$x)
emmeans(m0_reml, "trt", by="x", at=list(x=c(11, x=mean(bib$x))))

emmeans(m0_reml, "trt", by="x", at=list(x=c(quantile(bib$x,.25), mean(bib$x), quantile(bib$x,.75))))


pairs(emmeans(m0_reml, ~ trt, at = list(x =  quantile(bib$x,.25) )))
pairs(emmeans(m0_reml, ~ trt, at = list(x =  mean(bib$x) )))
pairs(emmeans(m0_reml, ~ trt, at = list(x =  quantile(bib$x,.75) )))
#----end-----------