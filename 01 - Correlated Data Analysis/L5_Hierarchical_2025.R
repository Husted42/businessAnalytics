#Lecture 5. Hierarchical models.
#Hierarchical models (eNote5, and eNote13 for the data).
# The random factors can be crossed or nested.
# Here we illustrate the case where the nested factors are nested.

#I Exploratory analysis
# II Fit model ignoring random effects.
# III Fit an initial model considering fixed and random effects.
# IV Check model fit of the final proposed model-residuals plots
# V # Visualize random effects
# VI Compute predicted values: full(fixed+random), only fixed, only random (BLUPS)
#  VII. Post-hoc analysis.

# The final proposed model did include the two fixed effects plus their interaction.
# It did include the factor effect ´pig´ only, ´litter´was statistically non significant.

# As part of a larger study of the intestinal health in newborn piglets,
# the gut enzyme lactase was measured in 20 piglets taken from 5 different litters.
# For each of the 20 piglets the lactase level was measured in three different regions.
# At the time the measurement was taken the piglet was either unborn (status=1) or newborn (status=2). 

# Number of observations: 60
# Fixed effects (by experimental design): region and status
# Random effects (experimental setting): litter and  pig
#Response variable: logclact

remove(list=ls())

library(lme4) #main library for LMM
library(lmerTest)  #library based on lme4 with approximate p-values for fixed effects.
library(predictmeans) # for diagnostic plots (residuals)
library(lattice) # to visualize random effects
library(emmeans) # for getting average estimated response values
library(multcomp) # For multiple testing 

getwd()

lactase <- read.table("https://www2.compute.dtu.dk/courses/02429/Data/datafiles/lactase.txt",
                      sep = ",", header = TRUE, na.strings = ".")
str(lactase)

#I. Exploratory analysis: tables and Plots
lactase$litter <- factor(lactase$litter)
lactase$pig <- factor(lactase$pig)
lactase$reg <- factor(lactase$reg)
lactase$status <- factor(lactase$status)
summary(lactase)
# Plots missing!
lactase$pig
#OBS that pig is already coded from 1:20, a different identifier for a different pig.
# So it is not necessary to create a new factor.

str(lactase)
xtabs(~ litter, lactase) #unbalanced
xtabs(~ pig, lactase)  #balanced
xtabs(~ reg, lactase) #balanced
xtabs(~ status, lactase) #unbalanced
xtabs(~ litter + reg, lactase)
xtabs(~ litter + pig, lactase)


# II Fit model ignoring random effects.
#Fit a model that ignores the structure of the experimental data
# It ignores the randomization stages

m0<-lm(loglact ~ reg + status + reg:status, data = lactase)
drop1(m0, test="F") #the interaction reg:status is not statistically significant.
anova(m0)
residplot(m0)
par(mfrow=c(2,2))
plot(m0, pch=19, cex=.7, col="blue")
par(mfrow=c(1,1))
#A reasonable fit. Though the interaction is non significant.

# III Fit an initial model considering fixed and random effects.
# Fit a starting model that considers the random structure
(m1_REML<- lmer(loglact ~ reg + status + reg:status + (1|litter/pig), data = lactase))
#Observe the random factors in the output:
# Random effects:
#   Groups     Name        Std.Dev.
# pig:litter (Intercept) 0.3345  
# litter     (Intercept) 0.0000  
# Residual               0.3673  

# It appears litter and pig:litter
#---Technical note on fitting hierarchical models in R-----
#Alternative code for fitting the same model:

(m1B_REML<- lmer(loglact ~ reg + status + reg:status + (1|litter + pig:litter), data = lactase))

(m1C_REML<- lmer(loglact ~ reg + status + reg:status + (1|litter + pig), data = lactase)) #pig is
# coded in a uniqueform for each of the 20 pigs (1,...,20). 

(m1D_REML<- lmer(loglact ~ reg + status + reg:status +  (1|pig) + (1|litter), data = lactase))

#---End Technical note -----

drop1(m1_REML) # the interaction of the two fixed effects is statistically significant.
anova(m1_REML)
ranova(m1_REML) # p-value/2 = .5 -->
# Does not reject the gypothesis H_0:sigma_litter^2=0 vs H_a: sigma_litter^2>0.
## The random effect litter is not statistically significant.
# Remove random effect litter.

(m2_REML<- lmer(loglact ~ reg + status + reg:status + (1|pig), data = lactase))
drop1(m2_REML) # The interaction term of the fixed effects still signifficant.
anova(m2_REML)
ranova(m2_REML) # Random effect statistically significant.
summary(m2_REML)

# IV Check model fit of the final proposed model-residuals plots

residplot(m2_REML) # Reasonably, though it could be improved.

# V. Visualize random effects
ranef(m2_REML) # predicted values for random effects (BLUPS), ploted bellow.
dotplot(ranef(m2_REML, condVar=TRUE), strip = FALSE, ylab="pig number") 

# VI Compute predicted values: full(fixed+random), only fixed, only random (BLUPS)

fixef(m2_REML)
ranef(m2_REML)

#the predict() function by default returns the same values as fitted(), but also
# returns the predicted values for only the fixed effects (Population level)
# Notice that observation number 24 is NA.

lactase$fitted_resp<-matrix(NA,nrow =60)
lactase$fitted_resp[1:23]<-fitted(m2_REML)[1:23] #fitted values considering fixed and random effects
lactase$fitted_resp[25:60]<-fitted(m2_REML)[24:59]

lactase$predicted_resp<-matrix(NA,nrow =60)
lactase$predicted_resp[1:23]<-predict(m2_REML, re.form=NULL)[1:23] #fitted values considering fixed and random effects
lactase$predicted_resp[25:60]<-predict(m2_REML, re.form=NULL)[24:59]

lactase$predicted_resp_fix<-matrix(NA,nrow =60)
lactase$predicted_resp_fix[1:23]<-predict(m2_REML, re.form=NA)[1:23] #fitted values considering fixed effects only
lactase$predicted_resp_fix[25:60]<-predict(m2_REML, re.form=NA)[24:59]
str(lactase)
lactase

ranef(m2_REML)
str(ranef(m2_REML))

(ranef_pig<-ranef(m2_REML)$pig$'(Intercept)')
(pig<-c(1:20))

df<-as.data.frame(cbind(pig,ranef_pig))
str(df)  
df$pig<- as.factor(df$pig)


lactase_2<-merge(lactase, df, by="pig",all = TRUE) #Notice the order according to pig
head(lactase_2)
2.129291 -0.17600554 #=1.953285. #first observation
2.120631 + 0.35715158 #=2.477783. #las observation.


# VII. Post-hoc analysis 
m2_REML. #2.12929 loglactose average value for reg=1, and Status=1.

options(digits = 6)
emmeans(m2_REML, "reg", by = c("status"))
emmeans(m2_REML, "reg") #Marginal means
emmeans(m2_REML, "status") #Marginal means

# Multiple testing and CI plots
library(multcomp)
mult_reg <- glht(m2_REML, linfct = mcp(reg = "Tukey")) #adjusted p-values by Tukey´s method
mult_status <- glht(m2_REML, linfct = mcp(status = "Tukey")) #adjusted p-values by Tukey´s method
#OBSERVE:
#Warning message:
#In mcp2matrix(model, linfct = linfct) :
#  covariate interactions found -- default contrast might be inappropriate


summary(mult_reg)
summary(mult_status)
anova(m2_REML)

par(mfrow=c(1,2))
par(mai=c(1,.65,1.25,.5)) # Use sufficiently large upper margin
plot(mult_reg, col=2:7)
plot(mult_status, col=2:7)
par(mai=c(1,1,1,1))
# No difference has been detected with the adjusted p-values!
# The interaction term reg:status makes the multi-comps unreliable,
# See warning.
#---end-----------------------------

