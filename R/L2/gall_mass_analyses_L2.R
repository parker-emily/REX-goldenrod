# TITLE:          REX: Gall mass analyses
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Moriah Young, Kristin Wolford, Emily Parker, Mark Hammond
# DATA INPUT:     Data imported as csv files from shared REX Google drive T7_warmx_insect L1 folder
# DATA OUTPUT:    analyses
# PROJECT:        REX
# DATE:           Jan 2022; updated June 2024

# Clear all existing data
rm(list=ls())

# Load packages
library(lmerTest)
library(fitdistrplus)
library(sjPlot)
library(tidyverse)
library(car)
library(emmeans)
library(bbmle)
library(multcomp)
library(knitr)

# Set working directory
dir<-Sys.getenv("DATA_DIR")

# Read in data
weight <- read.csv(file.path(dir, "T7_warmx_insect/L1/T7_warmx_Soca_galls_weight_L1.csv"))
weight <- weight %>%
  filter(!(Climate_Treatment == "Irrigated Control"))




######## Gall mass analysis ########
# Data exploration
descdist(weight$Dried_Weight, discrete = FALSE)
hist(weight$Dried_Weight)
qqnorm(weight$Dried_Weight)
shapiro.test(weight$Dried_Weight)
# pretty right skewed, going to try a few transformations & poisson distribution

# square root transformation
weight$sqrt_weight <- sqrt(weight$Dried_Weight)
descdist(weight$sqrt_weight, discrete = FALSE)
hist(weight$sqrt_weight)
qqnorm(weight$sqrt_weight)
shapiro.test(weight$sqrt_weight)

# log transformation
weight$log_weight <- log(weight$Dried_Weight)
descdist(weight$log_weight, discrete = FALSE)
hist(weight$log_weight)
qqnorm(weight$log_weight)
shapiro.test(weight$log_weight) # sqrt was better

# Assumption checking - log transformation
m1 <- lmer(sqrt(Dried_Weight) ~ Climate_Treatment + (1|Rep/Footprint/Subplot) + (1|Year), data = weight, REML=FALSE)
# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(m1, main = "Gall weight")
# Homogeneity of variance is ok here (increasing variance in resids is not increasing with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m1) ~ weight$Climate_Treatment)
# Assumption met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m1), main = "Gall weight")
hist(residuals(m1), main = "Gall weight")
shapiro.test(resid(m1))
outlierTest(m1)

# Model outcomes
anova(m1)
kable(anova(m1)) %>% kableExtra::kable_styling()
summary(m1)




######## Gall mass plotting ########
# Extract back-transformed EMMs
emm <- emmeans(m1, ~ Climate_Treatment, type = "response")
emm_df <- as.data.frame(emm)

ggplot(weight, aes(x=Climate_Treatment, y = Dried_Weight)) +
  geom_jitter(alpha = 0.3, color = "purple4") +
  labs(x = NULL, y = "Dried gall biomass (g)", title=NULL) +
  geom_point(data = emm_df, 
             aes(x = Climate_Treatment, y = response), 
             shape = 21, size = 3, fill = "purple4", position = position_dodge(width = 0.9)) +
  geom_errorbar(data = emm_df, 
                aes(x = Climate_Treatment, y = response, ymin = lower.CL, ymax = upper.CL), 
                width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed &\nDrought")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 16),
               axis.title = element_text(size=16,face="bold"))


