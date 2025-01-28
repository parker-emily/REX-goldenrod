# TITLE:          REX: Soca plant height analyses
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Moriah Young, Kristin Wolford, Emily Parker, Mark Hammond
# DATA INPUT:     Data imported as csv files from shared REX Google drive T7_warmx_plant_traits L1 folder
# DATA OUTPUT:    analyses
# PROJECT:        REX
# DATE:           Jan. 2022; updated Oct. 2023

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
height <- read.csv(file.path(dir, "/T7_warmx_plant_traits/L1/T7_warmx_soca_height_harvest_L1.csv"))

# remove irrigated control treatement for analysis
height <- height %>% filter(Climate_Treatment != "Irrigated Control")

###### Data exploration #######
# The first thing we do is check the distribution of the raw data
# We're checking for a normal distribution; the distribution of the raw data
# doesn't matter as much as the distribution of a model's residuals (which we'll also check),
# but its a good way to get a glimpse of how the data look
descdist(height$Height_cm, discrete = FALSE) # checks what type of distribution your data matches
hist(height$Height_cm) # should be a bell curve if normal
qqnorm(height$Height_cm) # should be a straight diagonal line if normal
shapiro.test(height$Height_cm) # p > 0.05 if normal
# making a model to test distribution of model residuals
raw.data.test <- lm(Height_cm ~ Climate_Treatment * Galling_Status, data=height) # testing model
hist(resid(raw.data.test)) # checking model residuals
shapiro.test(resid(raw.data.test))
# looks pretty good, going to try some transformations but might not need them

# log transformation
height$log_height <- log(height$Height_cm)
descdist(height$log_height, discrete = FALSE)
hist(height$log_height)
qqnorm(height$log_height)
shapiro.test(height$log_height) # looks good
# making a model to test distribution of model residuals w/ log transformation
raw.data.test.log <- lm(log_height ~ Climate_Treatment * Galling_Status, data=height) # testing model
hist(resid(raw.data.test.log)) # checking model residuals
shapiro.test(resid(raw.data.test.log))
# better - going with log transformation



######### Assumption checking ##########
# in the assumption checking, we're making sure that our full model meets the assumptions of the model.
# the model below is the model structure we can use for all response variables; its testing to see
# if there is 1. an effect of climate treatment on height, 2. an effect of galling status on height, and 3. does the effect
# of climate on height depend on galling status. Subplot nested within footprint nested within rep is used as our random effect
# to account for variation between plots. Year is also included as a random effect to account for variation between years.
m1 <- lmer(log(Height_cm) ~ Climate_Treatment * Galling_Status + (1|Rep/Footprint/Subplot) + (1|Year), data = height, REML=F)
# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(m1, main = "Plant height")
# Homogeneity of variance looks a bit off (increasing variance in resids does increase with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m1) ~ height$Climate_Treatment) # met
leveneTest(residuals(m1) ~ height$Galling_Status) # not met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m1), main = "Plant height")
hist(residuals(m1), main = "Plant height")
shapiro.test(resid(m1))
outlierTest(m1) # checking for outliers - none



###### Checking model results ########
anova(m1) # Interactive effect between climate and galling

# Pairwise comparisons
contrast(emmeans(m1, ~Climate_Treatment*Galling_Status), "pairwise", simple = "each", combine = F, adjust = "mvt")




######## Height plotting ########
# Extract back-transformed EMMs
emm <- emmeans(m1, ~ Climate_Treatment*Galling_Status, type = "response")
emm_df <- as.data.frame(emm)

png("plant_height.png", units="in", width=6, height=4, res=300)
ggplot(height, aes(x=Climate_Treatment, y = Height_cm, color = Galling_Status, fill = Galling_Status)) +
  geom_point(size=1, position=position_jitterdodge(), alpha=0.4) +
  geom_errorbar(data = emm_df, 
                aes(x = Climate_Treatment, y = response, ymin = response-SE, ymax = response+SE), 
                width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
  geom_point(data = emm_df, 
             aes(x = Climate_Treatment, y = response), 
             shape = 21, size = 3, color = "black", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  scale_color_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  labs(x = NULL, y = "Plant height (cm)", title=NULL) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed &\nDrought")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"),
        legend.title = element_text(size=11,face="bold"),
        legend.text = element_text(size=11))
dev.off()





######## Year plot for supp ########
m2 <- lmer(log(Height_cm) ~ Climate_Treatment * Galling_Status + Year + (1|Rep/Footprint/Subplot), data = height, REML=F)
# Extract back-transformed EMMs
emm2 <- emmeans(m2, ~ Climate_Treatment*Galling_Status+Year, type = "response")
emm_df2 <- as.data.frame(emm2)

png("plant_height_year.png", units="in", width=10, height=6, res=300)
ggplot(height, aes(x=Climate_Treatment, y = Height_cm, color = Galling_Status, fill = Galling_Status)) +
  facet_wrap(.~Year) +
  geom_point(size=1, position=position_jitterdodge(), alpha=0.4) +
  geom_errorbar(data = emm_df2, 
                aes(x = Climate_Treatment, y = response, ymin = response-SE, ymax = response+SE), 
                width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
  geom_point(data = emm_df2, 
             aes(x = Climate_Treatment, y = response), 
             shape = 21, size = 3, color = "black", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  scale_color_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  labs(x = NULL, y = "Plant height (cm)", title=NULL) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed &\nDrought")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"),
        legend.title = element_text(size=11,face="bold"),
        legend.text = element_text(size=11))
dev.off()

