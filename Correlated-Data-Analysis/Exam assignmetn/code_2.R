# Differnt measurments on the same unit

library(lme4)
###### ----- Load data ----- #####
setwd("/home/husted42/Documents/github/businessAnalytics/Correlated Data/Exam assignmetn")

getwd()
list.files()

rats_short <- read.table("Rats_Box.txt", header = 1, sep = ",")

str(rats_short)

# Convert to long format
library(tidyr)
rats <- pivot_longer(rats_short, 
                          cols = starts_with("y"), 
                          names_to = "Week", 
                          values_to = "Gain")

rats$Rat <- as.factor(rats$Rat)
rats$Trt <- as.factor(rats$Trt)
rats$Week <- as.factor(rats$Week)

rats <- rats %>% 
  filter(Week != "y0") %>%     # use only gain measurements
  mutate(
    Week_num = as.numeric(factor(Week, levels = c("y1","y2","y3","y4")))
  )


head(rats)

###### ----- Exploratory plots ----- ######
# Plot the with week on the x-axis and gain on the y-axis
library(ggplot2)
ggplot(rats, aes(x = Week, y = Gain, color = Trt, group = Rat)) +
  geom_line(alpha = 0.3) +
  geom_point() +
  stat_summary(aes(group = Trt), fun = mean, geom = "line", size = 1.5, color = "black") +
  stat_summary(aes(group = Trt), fun = mean, geom = "point", size = 3, color = "black") +
  labs(title = "Weight Gain of Rats Over Weeks by Treatment",
       x = "Week",
       y = "Weight Gain") +
  theme_minimal()

# graph the response for each rat as a function of time and for the mean values for the control and two treatment groups
ggplot() +
  geom_line(data = rats,
            aes(x = Week, y = Gain, group = Rat, color = Trt),
            alpha = 0.3) +
  geom_point(data = rats,
             aes(x = Week, y = Gain, color = Trt),
             alpha = 0.3) +

  geom_line(data = mean_rats,
            aes(x = Week, y = mean_Gain, color = Trt, group = Trt),
            size = 1.5) +
  geom_point(data = mean_rats,
             aes(x = Week, y = mean_Gain, color = Trt),
             size = 3) +
  labs(title = "Weight Gain of Rats Over Weeks by Treatment",
       x = "Week",
       y = "Weight Gain") +
  theme_minimal()

# Look deviation in gain
rats %>%
  filter(Week != "y0") %>%
  group_by(Trt, Week) %>%
  summarise(sd_gain = sd(Gain, na.rm = TRUE))

rats %>%
  filter(Week != "y0") %>%
  group_by(Trt) %>%
  summarise(sd_gain = sd(Gain, na.rm = TRUE))


# Plot the accumulated values
library(dplyr)
library(ggplot2)

# Per-rat cumulative gain
rats_cum <- rats %>%
  group_by(Rat, Trt) %>%
  arrange(Week, .by_group = TRUE) %>%
  mutate(cum_Gain = cumsum(Gain))

# Mean cumulative gain per treatment & week
mean_rats_cum <- rats_cum %>%
  group_by(Week, Trt) %>%
  summarise(mean_cum_Gain = mean(cum_Gain, na.rm = TRUE), .groups = "drop")

ggplot() +
  geom_line(data = rats_cum,
            aes(x = Week, y = cum_Gain, group = Rat, color = Trt),
            alpha = 0.3) +
  geom_point(data = rats_cum,
             aes(x = Week, y = cum_Gain, color = Trt),
             alpha = 0.3) +
  geom_line(data = mean_rats_cum,
            aes(x = Week, y = mean_cum_Gain, color = Trt, group = Trt),
            size = 1.5) +
  geom_point(data = mean_rats_cum,
             aes(x = Week, y = mean_cum_Gain, color = Trt),
             size = 3) +
  labs(title = "Cumulative Weight Gain of Rats Over Weeks by Treatment",
       x = "Week",
       y = "Cumulative Gain") +
  theme_minimal()


# Boxplot of Gain by Week and Treatment
ggplot(rats, aes(Week, Gain, fill = Trt)) +
  geom_boxplot(width = 0.2) +
  theme_minimal()

# Individual rat trajectories faceted by treatment
ggplot(rats, aes(Week, Gain, group = Rat, color = Rat)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ Trt) +
  theme_minimal()


# Separate analysis for each time–point
# We want to fit a model for each week separately
# Week 1
rats_gain <- rats %>% 
  filter(Week == "y1")

m1 <- lm(Gain ~ Trt, data = rats_gain)
anova(m1)

# Week 2
rats_gain <- rats %>% 
  filter(Week == "y2")

m2 <- lm(Gain ~ Trt, data = rats_gain)
anova(m2)

# Week 3
rats_gain <- rats %>% 
  filter(Week == "y3")

m3 <- lm(Gain ~ Trt, data = rats_gain)
anova(m3)

# Week 4
rats_gain <- rats %>% 
  filter(Week == "y4")

m4 <- lm(Gain ~ Trt, data = rats_gain)
anova(m4)


# summary statistic
## Average gain per rat (excluding baseline) 
rats_avg <- rats %>%
  filter(Week != "y0") %>%   # exclude baseline
  group_by(Rat, Trt) %>%
  summarise(avg_gain = mean(Gain), .groups = "drop")

m_avg <- lm(avg_gain ~ Trt, data = rats_avg)
anova(m_avg)

## Total increase
total_inc <- rats %>%
  filter(Week %in% c("y0", "y4")) %>%
  group_by(Rat, Trt) %>%
  summarise(
    total_increase = Gain[Week == "y4"] - Gain[Week == "y0"],
    .groups = "drop"
  )

m_inc <- lm(total_increase ~ Trt, data = total_inc)
anova(m_inc)

# Buidling a linear mixed model
# Model 0 - Random intercept for each rat
mll_0 <- lmer(Gain ~ 1 + (1|Rat), data = rats)

# Model 1 - Fixed effect for treatment 
mll_1 <- lmer(Gain ~ Trt + (1|Rat), data = rats)
summary(mll_1)
anova(mll_1)
ranova(mll_1)

# Model 2 - Adding week as a fixed effect
mll_2 <- lmer(Gain ~ Trt + Week + (1|Rat), data = rats)
summary(mll_2)
anova(mll_2)
ranova(mll_2)

# Model 3 - Interaction between treatment and week
mll_3 <- lmer(Gain ~ Trt * Week + (1|Rat), data = rats)
summary(mll_3)
anova(mll_3)
ranova(mll_3)

# Model 4 - Random slope for week per rat
mll_4 <- lmer(Gain ~ Trt * Week + (1 + Week_num|Rat), data = rats)
summary(mll_4)
anova(mll_4)
ranova(mll_4)

# We pick model 3
# Post-hoc analysis
library(emmeans)

# The standard error of the mean is calculated by $\sigma / \log(n)$.
# This result in a higher SE for group 2 since there are less rats.
emm_trt_week <- emmeans(mll_3, ~ Trt | Week)
emm_trt_week





