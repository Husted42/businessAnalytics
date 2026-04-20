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

# Playing with random slopes
m3_REML <- lmer(y ~ u.full + x + (x | county), data = radon)
ranova(m3_REML)
summary(m3_REML)

# Plot the random slope of m3
ranef_m3 <- ranef(m3_REML)$county
library(ggplot2)
ggplot(ranef_m3, aes(x = x, y = `(Intercept)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Random Slopes and Intercepts for Counties",
       x = "Slope for x",
       y = "Intercept") +
  theme_minimal()

# with u.full instead
# Playing with random slopes
m4_REML <- lmer(y ~ u.full + x + (u.full | county), data = radon)
ranova(m4_REML)
summary(m4_REML)

# Plot the random slope of m4
ranef_m4 <- ranef(m4_REML)$county
library(ggplot2)
ggplot(ranef_m4, aes(x = u.full, y = `(Intercept)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Random Slopes and Intercepts for Counties",
       x = "Slope for u.full",
       y = "Intercept") +
  theme_minimal()



# Plot the lines from the slopes and intecepts
library(ggplot2)
ggplot(radon, aes(x = x, y = y, color = as.factor(county))) +
  geom_point() +
  geom_abline(data = ranef_m3, aes(slope = x, intercept = `(Intercept)`, color = as.factor(row.names(ranef_m3)))) +
  labs(title = "Radon Levels by Floor and County",
       x = "Floor (x)",
       y = "Log Radon Level (y)",
       color = "County") +
  theme_minimal()


cor(radon$x, radon$u.full)
cor.test(radon$x, radon$u.full)

