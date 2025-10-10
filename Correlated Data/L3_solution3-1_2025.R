
# See eNote13, p 9 (and eNote3, p 23-24)

# 0. Read and prepare dataset.
# I. Exploratory analysis. Tables and plots
# II. Initial model
# III. Final proposed model
# For response a, the random factor was not statistically significant.
# Factor weeks was the only significant effect.
# Looking at the residual plots, I tried other models considering:
#    a) a transformation of the response: it did not work.
#    b) Removing the two observations with the largest response values.
#       obs number 16 and 17. I ended with the same model where only
#       weeks was statistically significant.
# IV. For b as response, one interaction was found statistically significant. 
#.    Analysis not presented here.

# Colour of spinage

# Key words:

#   Factor structure, Randomized blocks with three-way full factorial treatment structure.

# Description
# Spinage heated to 90 or 100 degrees Celcius was vacuum packed and stored for 0, 1 or
# 2 weeks before the packs were opened and chill stored in normal atmosphere for 0, 1 or
# 2 days. Then the colour was measured on a Hunter Lab. Two of the colour coordinates,
# a and b (measuring respectively something like red and yellow colour), were recorded
# and are given in the data set below. The variable batch is a blocking variable referring
# to two batches of spinage.

# Number of observations: 36

# Variable Description
 
# batch Values A,B (blocking factor)
# temp Values 90,100(celcius)
# weeks Values 0,1,2 (pre-storage time)
# days Values 0,1,2 (post-chill-storage time)
# a measure of red colour intensity
# b measure of yellow colour intensity

# Source
# The Royal Veterinary and Agricultural University, Denmark.

# Analysis
# Randomized block (fixed three-way treatment with interaction, random block) for each
# of the two variables.

# a) Write down all the factors relevant to the analysis, including their levels and mutu-
#   al structure. For example, are they crossed or nested? Make the factor structure
#   diagram.
#   
# b) Analyse the effect of the different factors on the two colour measurements, and
# summarize the significant effects (LS-means, etc.).

getwd()
remove(list=ls())

library(lme4)
library(lmerTest)
library(predictmeans)
library(MASS)

## Exercise 3.1

# 0. Read and prepare dataset.
#
spinage <- read.csv("https://www2.compute.dtu.dk/courses/02429/Data/datafiles/spinage.txt")
str(spinage) #36 Obs

spinage$batch<-factor(spinage$batch)
spinage$temp<-factor(spinage$temp)
spinage$weeks<-factor(spinage$weeks)
spinage$days<-factor(spinage$days)
str(spinage)

# I. Exploratory analysis. Tables and plots

summary(spinage[,1:4]) #all four factors are balanced
table(spinage$batch) #18 per batcg
table(spinage$temp) #18 per temp
table(spinage$weeks) #12 per week
table(spinage$days) #12 per day
#Alternatively:
replications(spinage[,1:4])

ftable(spinage$batch,spinage$temp,spinage$weeks,spinage$days)  #1 per cell
sum(ftable(spinage$batch,spinage$temp,spinage$weeks,spinage$days)) #36 obs
# exploratory plots

par(mai=c(.75,.75,.5,.75))
par(mfrow=c(2,2))
boxplot(a ~ batch, data = spinage, xlab='batch',
        ylab='a: measurment red', las=1, col=2:10)
boxplot(a ~ temp, data = spinage, xlab='temp',
        ylab='a: measurment red', las=1, col=2:10)
boxplot(a ~ weeks, data = spinage, xlab='weeks',
        ylab='a: measurment red', las=1, col=2:10)
boxplot(a ~ days, data = spinage, xlab='days',
        ylab='a: measurment red', las=1, col=2:10)

# Looking at the box plots, weeks appears to be the only factor that has an
# effect on the response a.
par(mfrow=c(2,2))
(means1 <- tapply(spinage$a, spinage$batch,  mean))
stripchart(a~batch, data=spinage, vertical=TRUE, method="jitter", xlab="batch", 
           ylab='a: measurment red',pch=16,cex=.75, las=1, col=2:4)
points(1:2, means1, pch = 17, cex = 1.5, col = "black")
abline(h = mean(spinage$a), lty = 2)

(means2 <- tapply(spinage$a, spinage$temp,  mean))
stripchart(a~temp, data=spinage, vertical=TRUE, method="jitter", xlab="temperature", 
           ylab='a: measurment red',pch=16,cex=.75, las=1, col=2:4)
points(1:2, means2, pch = 17, cex = 1.5, col = "black")
abline(h = mean(spinage$a), lty = 2)

(means3 <- tapply(spinage$a, spinage$weeks,  mean))
stripchart(a~weeks, data=spinage, vertical=TRUE, method="jitter", xlab="weeks", 
           ylab='a: measurment red',pch=16,cex=.75, las=1, col=2:4)
points(1:3, means3, pch = 17, cex = 1.5, col = "black")
abline(h = mean(spinage$a), lty = 2)

(means4 <- tapply(spinage$a, spinage$days,  mean))
stripchart(a~days, data=spinage, vertical=TRUE, method="jitter", xlab="days", 
           ylab='a: measurment red',pch=16,cex=.75, las=1, col=2:4)
points(1:3, means4, pch = 17, cex = 1.5, col = "black")
abline(h = mean(spinage$a), lty = 2)

# The strip plots show some points with response values outlying in the top.

par(mfrow=c(2,2))
plot(spinage$a, pch=19); plot(spinage$b, pch=19)
hist(spinage$a, breaks = 36)
hist(spinage$b, breaks = 36)
par(mfrow=c(1,1))

# II. Initial model
# "a" as response variable

m0<-lmer(a~temp+weeks+days+(1|batch),data=spinage) 
drop1(m0)
ranova(m0) 
residplot(m0) 
#Random effect is not statistically significant
#A transformation on the response might be useful?
#Two outying points? (Lect 6)


m1<-lmer(a~temp*weeks*days+(1|batch),data=spinage) #Too many parameters
drop1(m1)
ranova(m1) 
residplot(m1)
dotplot(ranef(m1, condVar=TRUE), strip = FALSE, ylab="batch number")
#Random effect is not statistically significant

# Try an automatized model selection as a quick look

m1_step<- step(m1)
m1_step # Only one fixed factor seems statistically significant, weeks
        # and no random effect.
#Model found:
#  a ~ weeks


# I tried a transformation on the response variable without success,
#  using function boxcox() in library(MASS).

# I tried to remove obs 16 and 17 whose response variable take the largest values:
# -5.5, -.6 resp. Using file spinage[-c(16:17),], I arrived to the same model: 
# only weeks was statistically significant.

spinage[spinage$a>-6,]
# batch temp weeks days     a     b
# 16     A  100     2    0 -5.28 10.41
# 17     A  100     2    1 -5.71  9.72


#------
# Fit the saturated model and remove non significant terms one at the time.

m2<-lm(a~temp*weeks*days,data=spinage)
drop1(m2, test="F")
residplot(m2)

m3<-update(m2, .~.- temp:weeks:days)
drop1(m3, test="F")

m4<-update(m3,.~.- temp:weeks)
drop1(m4, test="F")

m5<-update(m4,~.- weeks:days)
drop1(m5,test="F")

m6<-update(m5,~.- temp:days)
drop1(m6,test="F")

m7<-update(m6,~.- temp)
drop1(m7,test="F")


m8<-update(m7,~.- days)
drop1(m8,test="F")
summary(m8)

# III. Final proposed model
formula(m8)
#a ~ weeks

# The average response value of the colour red measurement varies according
# to weeks, the difference lies between weeks 2 and 0.

#-------
# Pos-hoc analyis

library(emmeans)

emmeans(m8, "weeks")

library(multcomp)
mult_weeks <- glht(m8, linfct = mcp(weeks = "Tukey")) 

summary(mult_weeks)

plot(mult_weeks, col=2:4)

#--------end----

