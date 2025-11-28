library(dplyr)
library(emmeans)
library(multcomp)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(predictmeans)

library(lme4)
library(predictmeans)

setwd("/home/husted42/Documents/github/businessAnalytics/Correlated Data/Exam assignmetn")

getwd()
list.files()

radon <- read.table("Radon_MN.csv", header = 1)

head(radon)


unique(radon$county.name)
table(radon$county.name)
table(radon$idnum)

# How many regions per county
radon %>%
  group_by(county, region) %>%
  summarise() %>%                   # keep unique county–region pairs
  count(county, name = "n_regions") %>%
  arrange(desc(n_regions))    

# County 
# Hiearchy : House within counties

# i : House
# x : Floor
# U : Uranimum
# y : log(Radon)

m1_REML<- lmer(y ~ u.full + x + (1|region/county), data = radon)
summary(m1_REML)

m2_REML<- lmer(y ~ u.full + x + (1|county), data = radon)
summary(m1_REML)

ranova(m2_REML)

residplot(m2_REML)

# Look at row 209
radon[209:211,]

# predict radon[209:211,] with intervals
predict(m2_REML, newdata = radon[209:211,], re.form = NULL)
emmeans(m2_REML, specs = "x")


radon[300:305,]
predict(m2_REML, newdata = radon, re.form = NULL)



