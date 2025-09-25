
sex <- factor(c(rep("female", 4), rep("male", 4)))
tmt <- factor(c(0, 0, 1, 1, 0, 0, 1, 1))
y <- c(-9.27, -1.28, 3.98, 7.06, 1.02, -1.79, 3.64, 1.94)
summary(lm(y ~ sex * tmt ))


" 
  Intercept -> avg female treatment 0

  We are not able to estimate the last parameter. Because the model is over parameterized.
  ie. Have have more parameters beta than vairalbes y
"

  ##### ----- Testing random effects ----- #####
library(lmerTest)

planks <- read.csv("https://www2.compute.dtu.dk/courses/02429/Data/datafiles/planks.txt")
planks

planks$plank<-factor(planks$plank)
planks$depth<-factor(planks$depth)
planks$width<-factor(planks$width)
planks$loghum <- log(planks$humidity)

# We want to consider two random factors depth an width
# We want to check the hypothesis that depth doesn't have a significant
# Impact on the model.
# To do this by making a model and a sub-model.

# \sigma_{planks*depth} = 0

model4 <- lmer(loghum ~ depth * width + (1|plank) + (1|depth:plank) +
                 (1|plank:width), data = planks)

model4.1 <- lmer(loghum ~ depth * width + (1|plank) + (1|plank:width),
                data = planks)
anova(model4, model4.1, refit = FALSE)

# With a p-value of 0.10083 we do not reject the H_0 hypothesis on a 5% significance level
# There is not evidence that including this random effect will improve our model

# Similarly for the other interaction random coponent
# \sigma_{planks*width} = 0

model4.2 <- lmer(loghum ~ depth * width + (1|plank) + (1|depth:plank),
                 data = planks)
anova(model4, model4.2, refit = FALSE)

# We reject the 0-hypothis on a significance level on 5% with a p-value of 2.2e-16
# This means that there is evidence that we are able to improve the model by including the random effect plank width
"
We compared nested linear mixed-effects models to assess the contribution of random interaction effects. 
Removing the random effect for depth × plank did not significantly worsen model fit (chi-squared = 2.58, df = 1, p = 0.11), 
indicating little evidence for depth-specific variability among planks. 
In contrast, removing the random effect for plank × width resulted in a substantial decrease in model fit (chi-squared = 111.85, df = 1, p < 2.2e-16), 
providing strong evidence that this source of variability should be retained in the model.
"

